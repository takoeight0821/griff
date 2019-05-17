#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <gc.h>
#include <stdint.h>

struct value;

struct stack
{
  struct value *ptr;
  size_t capacity;
  size_t len;
};

typedef void*(Code)(void);

enum value_tag {
  INTEGER,
  EPSILON,
  CLOSURE,
  RETURN,
  BLOCK,
};

struct value
{
  union {
    int integer;
    struct
    {
      Code *entry;
      struct stack env;
    } clos;
    struct
    {
      uint8_t tag;
      struct value* vec;
      size_t len;
    } block;
  };
  enum value_tag tag;
};

struct stack *ArgStack;
struct stack *RetStack;

#define CHUNK 16
void stack_init() {
  ArgStack = malloc(sizeof(struct stack));
  ArgStack->ptr = calloc(CHUNK, sizeof(struct value));
  ArgStack->capacity = CHUNK;
  ArgStack->len = 0;
  RetStack = malloc(sizeof(struct stack));
  RetStack->ptr = calloc(CHUNK, sizeof(struct value));
  RetStack->capacity = CHUNK;
  RetStack->len = 0;
}

struct stack *Env;

void env_init() {
  Env = malloc(sizeof(struct stack));
  Env->ptr = calloc(CHUNK, sizeof(struct value));
  Env->capacity = CHUNK;
  Env->len = 0;
}

void dump_value(struct value v)
{
  switch (v.tag) {
  case INTEGER:
    printf("%d", v.integer);
    break;
  case CLOSURE:
    printf("%p", v.clos.entry);
    break;
  case RETURN:
    printf("ret");
    break;
  case EPSILON:
    printf("epsilon");
    break;
  case BLOCK:
    printf("(%d ", v.block.tag);
    for (size_t i = 0; i < v.block.len; i++) {
      if (i != 0)
        printf(", ");
      dump_value(v.block.vec[i]);
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

  for (size_t i = 0; i < s.len; i++)
  {
    if (i != 0)
      printf(", ");
    dump_value(s.ptr[i]);
  }

  printf("]");
}

void dump_env(struct stack env)
{
  printf("ENV (%lu)(%lu): ", env.len, GC_size(env.ptr));
  for (size_t i = 0; i < env.len; i++)
  {
    printf("%d, ", env.ptr[i].integer);
  }
  printf("\n");
}

void push(struct stack *s, struct value value)
{
  // メモリが足りなくなったらcapacity*2
  if (s->len >= s->capacity) {
    s->capacity *= 2;
    s->ptr = realloc(s->ptr, s->capacity * sizeof(struct value));
  }

  s->ptr[s->len++] = value;
}

struct value pop(struct stack *s)
{
  return s->ptr[--s->len];
}

void push_env(struct stack *env, struct value value)
{
  push(env, value);
}

void ldi(int val)
{
  struct value value = {
    .integer = val,
    .tag = INTEGER,
  };
  push(ArgStack, value);
}

void access(size_t i)
{
  push(ArgStack, Env->ptr[Env->len - i - 1]);
}

struct stack copy_env(struct stack old)
{
  struct stack new_env;
  new_env.ptr = GC_MALLOC(CHUNK * sizeof(struct value));
  new_env.capacity = CHUNK;
  new_env.len = 0;
  for (size_t i = 0; i < old.len; i++)
  {
    push_env(&new_env, old.ptr[i]);
  }
  return new_env;
}

struct value new_closure(Code *f, struct stack env)
{
  struct value c = {
    .clos = {
      .entry = f,
      .env = copy_env(env),
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
  Env->len--;
}

Code* test(Code *c1, Code *c2)
{
  if (pop(ArgStack).integer)
  {
    return c1;
  }
  else
  {
    return c2;
  }
}

void add(void)
{
  int n1 = pop(ArgStack).integer;
  int n2 = pop(ArgStack).integer;
  struct value n3 = {
    .integer = n1 + n2,
    .tag = INTEGER,
  };
  push(ArgStack, n3);
}

void eq(void)
{
  int n1 = pop(ArgStack).integer;
  int n2 = pop(ArgStack).integer;
  struct value n3 = {
    .integer = n1 == n2,
    .tag = INTEGER,
  };
  push(ArgStack, n3);
}

void make_block(uint_fast8_t tag, size_t len) {
  struct value ret =
    {
      .tag = BLOCK,
      .block =
      {
        .vec = GC_MALLOC(sizeof(struct value) * len),
        .len = len,
        .tag = tag,
      },
    };
  for (size_t i = 0; i < len; i++) {
    ret.block.vec[i] = pop(ArgStack);
  }
  push(ArgStack, ret);
}

void field(size_t i)
{
  struct value block = pop(ArgStack);
  push(ArgStack, block.block.vec[i]);
}

struct value peek(struct stack s) {
  // return last item
  return s.ptr[s.len - 1];
}

Code* invoke(uint_fast8_t tag, Code* cont1, Code* cont2) {
  if (peek(*ArgStack).block.tag == tag) {
    return cont1;
  } else {
    return cont2;
  }
}

Code* apply(Code* cont)
{
  struct value closure = pop(ArgStack);
  struct value val = pop(ArgStack);

  struct value save = new_closure(cont, *Env);
  save.tag = RETURN;
  push(RetStack, save);

  /* printf("apply: "); */
  /* dump_stack(*RetStack); */
  /* printf("\n"); */

  *Env = copy_env(closure.clos.env);

  push_env(Env, closure);
  push_env(Env, val);

  return closure.clos.entry;
}

Code* tail_apply(void)
{
  struct value closure = pop(ArgStack);
  struct value val = pop(ArgStack);
  *Env = copy_env(closure.clos.env);

  push_env(Env, closure);
  push_env(Env, val);

  /* printf("tail apply\n"); */
  /* dump_stack(*ArgStack); */
  /* printf("\n"); */
  /* dump_env(*Env); */
  /* printf("%lu %lu %lu\n\n", GC_get_gc_no(), GC_get_heap_size(), GC_get_free_bytes()); */

  return closure.clos.entry;
}

struct value make_epsilon() {
  struct value e = {.tag = EPSILON };
  return e;
}

void push_mark(void)
{
  push(ArgStack, make_epsilon());
}

Code* grab(Code *cont)
{
  struct value v = pop(ArgStack);

  if (v.tag == EPSILON)
  {
    struct value ret = pop(RetStack);

    /* printf("grab: "); */
    /* dump_stack(*RetStack); */
    /* printf("\n"); */

    struct value v = new_closure(cont, *Env);

    *Env = ret.clos.env;
    push(ArgStack, v);
    return ret.clos.entry;
  }
  else
  {
    struct value closure = new_closure(cont, *Env);
    *Env = copy_env(closure.clos.env);
    push_env(Env, closure);
    push_env(Env, v);
    return cont;
  }
}

Code* return_clos(void)
{
  struct value x = pop(ArgStack);
  struct value y = pop(ArgStack);

  if (y.tag == EPSILON)
  {
    struct value ret = pop(RetStack);
    *Env = ret.clos.env;
    push(ArgStack, x);
    return ret.clos.entry;
  }
  else
  {
    *Env = copy_env(x.clos.env);
    push_env(Env, x);
    push_env(Env, y);
    return x.clos.entry;
  }
}

Code* c1(void)
{
  access(0);
  return return_clos();
}

Code* c2(void)
{
  access(0);
  access(2);
  add();
  ldi(-1);
  access(2);
  add();
  access(3);
  return tail_apply();
}

Code* f_cont(void)
{
  ldi(0);
  access(2);
  eq();
  return test((Code*)c1, (Code*)c2);
}

Code* f(void)
{
  return grab((Code*)f_cont);
}

Code* end(void) {
  endlet();
  dump_stack(*ArgStack);
  exit(0);
}

Code* entry(void)
{
  closure((Code*)f);
  let();
  push_mark();
  ldi(0);
  ldi(100);
  access(0);
  return apply((Code*)end);
}

Code* invoke_test(void)
{
  dump_stack(*ArgStack);
  exit(0);
}

Code* block_test_entry(void)
{
  ldi(3);
  ldi(2);
  ldi(1);
  make_block(0, 2);
  return invoke(0, (Code*)invoke_test, NULL);
}

Code* when_fail(void){
  printf("unreachable¥n");
  exit(1);
}

Code* when_nil(void)
{
  struct value v = pop(ArgStack);
  printf("NIL: ");
  dump_value(v);
  printf("\n");
  exit(0);
}

Code* when_cons(void)
{
  struct value v = pop(ArgStack);
  printf("CONS: ");
  dump_value(v);
  printf("\n");
  exit(0);
}

Code* next_nil(void)
{
  return invoke(1, (Code*)when_cons, (Code*) when_fail);
}

Code* cons_entry(void)
{
  make_block(0, 0);
  ldi(42);
  make_block(1, 2);
  return invoke(0, (Code*)when_nil, (Code*)next_nil);
}


int main()
{
  GC_INIT();

  stack_init();

  env_init();

  Code* cont = (Code*)cons_entry;
  while (true) {
    cont = cont();
  }
  /* entry(); // 50005000 */
  /* block_test_entry(); // simple invoke test */
  // cons_entry(); // list
}
