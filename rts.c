#include <emscripten.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>

typedef struct FunctionClosure_ {
    void *function;
} FunctionClosure;

// int return type because we don't support not returning! Unit is 0.
int64_t mc_print_int_f(FunctionClosure *self, int64_t i) {
    printf("%" PRId64, i);
    return 0;
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_print_int = { .function = &mc_print_int_f };

int64_t mc_print_newline_f(FunctionClosure *self, int64_t i) {
    printf("\n");
    return 0;
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_print_newline = { .function = &mc_print_newline_f };

double mc_float_of_int_f(FunctionClosure *self, int64_t i) {
    return (double)i;
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_float_of_int = { .function = &mc_float_of_int_f };

int64_t mc_int_of_float_f(FunctionClosure *self, double d) {
    return (int64_t)d;
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_int_of_float = { .function = &mc_int_of_float_f };

// truncate = int_of_float
EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_truncate = { .function = &mc_int_of_float_f };

double mc_abs_float_f(FunctionClosure *self, double d) {
    return fabs(d);
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_abs_float = { .function = &mc_abs_float_f };

double mc_sqrt_f(FunctionClosure *self, double d) {
    return sqrt(d);
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_sqrt = { .function = &mc_sqrt_f };

double mc_sin_f(FunctionClosure *self, double d) {
    return sin(d);
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_sin = { .function = &mc_sin_f };

double mc_cos_f(FunctionClosure *self, double d) {
    return cos(d);
}

EMSCRIPTEN_KEEPALIVE
FunctionClosure mc_cos = { .function = &mc_cos_f };
