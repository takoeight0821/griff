#define GC_DEBUG
#include <stdlib.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <gc.h>
#include <stdint.h>

struct value;

struct env
{
  struct value *array;
  size_t length;
};

typedef void(Code)(void);

#define INTEGER 0
#define EPSILON 1
#define CLOSURE 2
#define RETURN 3
#define BLOCK 4
#define MEMTERM 5

struct value
{
  union {
    int integer;
    struct
    {
      Code *entry;
      struct env env;
    } clos;
    struct
    {
      struct value* vec;
      size_t len;
      uint_fast8_t tag;
    } block;
  } value;
  uint_fast8_t tag;
};

struct stack
{
  struct value *base;
  struct value *curr;
};

struct stack *ArgStack;
struct stack *RetStack;

struct value make_memterm() {
  struct value e = {.tag = MEMTERM };
  return e;
}

#define CHUNK 16
#define MARGIN (CHUNK * 2)


void stack_init() {
  ArgStack = malloc(sizeof(struct stack));
  ArgStack->base = calloc(CHUNK + 1, sizeof(struct value));
  ArgStack->curr = ArgStack->base;
  ArgStack->base[CHUNK] = make_memterm();
  RetStack = malloc(sizeof(struct stack));
  RetStack->base = calloc(CHUNK + 1, sizeof(struct value));
  RetStack->curr = RetStack->base;
  RetStack->base[CHUNK] = make_memterm();
}

struct env *Env;

void env_init() {
  Env = malloc(sizeof(struct env));
  Env->array = GC_MALLOC(sizeof(struct value) * (CHUNK + 1));
  Env->length = 0;
  Env->array[CHUNK] = make_memterm();
}

void dump_value(struct value v)
{
  switch (v.tag) {
  case INTEGER:
    printf("%d", v.value.integer);
    break;
  case CLOSURE:
    printf("%p", v.value.clos.entry);
    break;
  case RETURN:
    printf("ret");
    break;
  case EPSILON:
    printf("epsilon");
    break;
  case BLOCK:
    printf("(%d ", v.value.block.tag);
    for (size_t i = 0; i < v.value.block.len; i++) {
      if (i != 0)
        printf(", ");
      dump_value(v.value.block.vec[i]);
    }
    printf(")");
    break;
  default:
    break;
  }
}

void dump_stack(struct stack s)
{
  printf("STACK [");

  for (size_t i = 0; i < s.curr - s.base; i++)
  {
    if (i != 0)
      printf(", ");
    dump_value(s.base[i]);
  }

  printf("]");
}

void dump_env(struct env env)
{
  printf("ENV (%lu)(%lu): ", env.length, GC_size(env.array));
  for (size_t i = 0; i < env.length; i++)
  {
    printf("%d, ", env.array[i].value.integer);
  }
  printf("\n");
}

void push(struct stack *s, struct value value)
{
  // メモリが足りなくなったら+CHUNK
  // s->currにすでにmemtermが入っていたらメモリ不足
  if (s->curr->tag == MEMTERM) {
    ptrdiff_t diff = s->curr - s->base;
    s->base = realloc(s->base, (diff + CHUNK + 1) * sizeof(struct value));
    s->curr = s->base + diff;
    s->base[diff + CHUNK] = make_memterm();
  }

  *s->curr = value;
  s->curr++;
}

struct value pop(struct stack *s)
{
  s->curr--;
  /* // メモリがMARGIN余ったら-CHUNK */
  /* if (s->size > MARGIN && s->curr - s->base < s->size - MARGIN - 1) */
  /* { */
  /*   s->size -= CHUNK; */
  /*   ptrdiff_t diff = s->curr - s->base; */
  /*   s->base = realloc(s->base, s->size * sizeof(struct value)); */
  /*   s->curr = s->base + diff; */
  /* } */

  return *s->curr;
}

void push_env(struct env *env, struct value value)
{
  if (env->array[env->length].tag == MEMTERM)
  {
    env->array = GC_REALLOC(env->array, sizeof(struct value) * env->length + CHUNK + 1);
    env->array[env->length + CHUNK] = make_memterm();
  }
  env->array[env->length] = value;
  env->length++;
}

void ldi(int val)
{
  struct value value = {
    .value = {.integer = val},
    .tag = INTEGER,
  };
  push(ArgStack, value);
}

void access(size_t i)
{
  push(ArgStack, Env->array[Env->length - i - 1]);
}

struct env copy_env(struct env old)
{
  struct env new_env;
  new_env.array = GC_MALLOC((CHUNK + 1) * sizeof(struct value));
  new_env.length = 0;
  new_env.array[CHUNK] = make_memterm();
  for (size_t i = 0; i < old.length; i++)
  {
    push_env(&new_env, old.array[i]);
  }
  return new_env;
}

struct value new_closure(Code *f, struct env env)
{
  struct value c = {
    .value = {
      .clos = {
        .entry = f,
        .env = copy_env(env),
      },
    },
    .tag = CLOSURE,
  };
  return c;
}

void closure(Code *f)
{
  struct value closure = new_closure(f, *Env);
  push(ArgStack, closure);
}

void let(void)
{
  struct value v = pop(ArgStack);
  push_env(Env, v);
}

void endlet(void)
{
  Env->length--;
}

void test(Code *c1, Code *c2)
{
  if (pop(ArgStack).value.integer)
  {
    c1();
  }
  else
  {
    c2();
  }
}

void add(void)
{
  int n1 = pop(ArgStack).value.integer;
  int n2 = pop(ArgStack).value.integer;
  struct value n3 = {
    .value = {.integer = n1 + n2},
    .tag = INTEGER,
  };
  push(ArgStack, n3);
}

void eq(void)
{
  int n1 = pop(ArgStack).value.integer;
  int n2 = pop(ArgStack).value.integer;
  struct value n3 = {
    .value = {.integer = n1 == n2},
    .tag = INTEGER,
  };
  push(ArgStack, n3);
}

void make_block(uint_fast8_t tag, size_t len) {
  struct value ret =
    {
      .tag = BLOCK,
      .value =
      {
        .block =
        {
          .vec = GC_MALLOC(sizeof(struct value) * len),
          .len = len,
          .tag = tag,
        },
      },
    };
  for (size_t i = 0; i < len; i++) {
    ret.value.block.vec[i] = pop(ArgStack);
  }
  push(ArgStack, ret);
}

void field(size_t i)
{
  struct value block = pop(ArgStack);
  push(ArgStack, block.value.block.vec[i]);
}

struct value peek(struct stack s) {
  return *(s.curr - 1);
}

void invoke(uint_fast8_t tag, Code* cont) {
  if (peek(*ArgStack).value.block.tag == tag) {
    cont();
  }
}

void apply(void)
{
  struct value closure = pop(ArgStack);
  struct value val = pop(ArgStack);

  struct value save = new_closure((Code *)0xdeadbeef, *Env);
  save.tag = RETURN;
  push(RetStack, save);

  /* printf("apply: "); */
  /* dump_stack(*RetStack); */
  /* printf("\n"); */

  *Env = copy_env(closure.value.clos.env);

  push_env(Env, closure);
  push_env(Env, val);

  closure.value.clos.entry();
}

void tail_apply(void)
{
  struct value closure = pop(ArgStack);
  struct value val = pop(ArgStack);
  *Env = copy_env(closure.value.clos.env);

  push_env(Env, closure);
  push_env(Env, val);

  /* printf("tail apply\n"); */
  /* dump_stack(*ArgStack); */
  /* printf("\n"); */
  /* dump_env(*Env); */
  /* printf("%lu %lu %lu\n\n", GC_get_gc_no(), GC_get_heap_size(), GC_get_free_bytes()); */

  closure.value.clos.entry();
}

struct value make_epsilon() {
  struct value e = {.tag = EPSILON };
  return e;
}

void push_mark(void)
{
  push(ArgStack, make_epsilon());
}

void grab(Code *cont)
{
  struct value v = pop(ArgStack);

  if (v.tag == EPSILON)
  {
    struct value ret = pop(RetStack);

    /* printf("grab: "); */
    /* dump_stack(*RetStack); */
    /* printf("\n"); */

    struct value v = new_closure(cont, *Env);

    *Env = ret.value.clos.env;
    push(ArgStack, v);
  }
  else
  {
    struct value closure = new_closure(cont, *Env);
    *Env = copy_env(closure.value.clos.env);
    push_env(Env, closure);
    push_env(Env, v);
    cont();
  }
}

void return_clos(void)
{
  struct value x = pop(ArgStack);
  struct value y = pop(ArgStack);

  if (y.tag == EPSILON)
  {
    *Env = pop(RetStack).value.clos.env;

    /* printf("return_clos: "); */
    /* dump_stack(*RetStack); */
    /* printf("\n"); */

    push(ArgStack, x);
  }
  else
  {
    *Env = copy_env(x.value.clos.env);
    push_env(Env, x);
    push_env(Env, y);
  }
}

void c1(void)
{
  access(0);
  return_clos();
}

void c2(void)
{
  access(0);
  access(2);
  add();
  ldi(-1);
  access(2);
  add();
  access(3);
  tail_apply();
}

void f_cont(void)
{
  ldi(0);
  access(2);
  eq();
  test(c1, c2);
}

void f(void)
{
  grab(f_cont);
}

void entry(void)
{
  closure(f);
  let();
  push_mark();
  ldi(0);
  ldi(100000);
  /* dump_stack(*ArgStack); */
  /* printf("\n"); */
  access(0);
  apply();
  endlet();

  dump_stack(*ArgStack);
}

void invoke_test(void)
{
  struct value v = pop(ArgStack);
  ldi(10);
}

void block_test_entry(void)
{
  ldi(3);
  ldi(2);
  ldi(1);
  make_block(0, 2);
  invoke(0, invoke_test);
  dump_stack(*ArgStack);
}

void when_nil(void)
{
  struct value v = pop(ArgStack);
  printf("NIL: ");
  dump_value(v);
  printf("\n");
}

void when_cons(void)
{
  struct value v = pop(ArgStack);
  printf("CONS: ");
  dump_value(v);
  printf("\n");
}

void cons_entry(void)
{
  make_block(0, 0);
  ldi(42);
  make_block(1, 2);
  invoke(0, when_nil);
  invoke(1, when_cons);
  ldi(0);
}

int main()
{
  GC_INIT();

  stack_init();
  env_init();

  entry(); // 50005000
  /* block_test_entry(); // simple invoke test */
  // cons_entry(); // list
}
