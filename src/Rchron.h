#ifndef R_CHRON_H
#define R_CHRON_H

#include <R.h>
#include <Rinternals.h>

#include <ctype.h>

SEXP R_unpaste(SEXP s_strings, SEXP s_sep, SEXP s_whitespace,
	       SEXP s_nfields);
void R_cnt_flds_str(char **strings, Sint *nstrings, char **sep,
		    Sint *white_space, Sint *counts);

#endif
