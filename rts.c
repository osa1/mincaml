#include <inttypes.h>
#include <stdio.h>

typedef struct FunctionClosure_ {
    void *function;
} FunctionClosure;

// int return type because we don't support not returning! Unit is 0.
int64_t print_int_fun(FunctionClosure *self, int64_t i) {
    printf("%" PRId64, i);
    return 0;
}

FunctionClosure print_int = { .function = &print_int_fun };
