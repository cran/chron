#include "Rchron.h"

#include <R_ext/Rdynload.h>

R_NativePrimitiveArgType cnt_flds_t[] =
{STRSXP, INTSXP, STRSXP, INTSXP, INTSXP};

R_CMethodDef cmethods[] = {
    {"cnt_flds_str", (DL_FUNC) &cnt_flds_str, 5, cnt_flds_t},
    {NULL, NULL, 0}
};

R_CallMethodDef callMethods[] = {
    {"unpaste", (DL_FUNC) &unpaste, 4},
    {NULL, NULL, 0}
};

void
R_init_chron(DllInfo *dll)
{
    R_registerRoutines(dll, cmethods, callMethods, NULL, NULL);
}

