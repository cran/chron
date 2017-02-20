#include "Rchron.h"

#include <R_ext/Rdynload.h>

R_NativePrimitiveArgType cnt_flds_t[] =
{STRSXP, INTSXP, STRSXP, INTSXP, INTSXP};

static const R_CMethodDef CEntries[] = {
    {"C_cnt_flds_str", (DL_FUNC) &C_cnt_flds_str, 5, cnt_flds_t},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"C_unpaste", (DL_FUNC) &C_unpaste, 4},
    {NULL, NULL, 0}
};

void
R_init_chron(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
