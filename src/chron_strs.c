#include <ctype.h>
#include <R.h>

/* count fields in vector of strings */

void
cnt_flds_str(char **strings, Sint *nstrings, char **sep, Sint
	     *white_space, Sint *counts)
{
    Sint n = *nstrings, whitespace = *white_space;
    Sint i, nsep;
    char *s, c_sep = **sep;
    int c, in_white_space;
  
    if(!whitespace) {
	/* sep is a one-char delimiter */
	for(i = 0; i < n; ++i) {
	    s = strings[i];
	    if(*s == '\0')
		continue;	/* empty string */
	    nsep = 0;
	    while((c = *s++) != '\0')
		if(c == c_sep)
		    ++nsep;
	    counts[i] = nsep + 1;
	}
	return;
    } 
    /* items are delimited by white space (new lines, tabs, spaces) */
    for(i = 0; i < n; ++i) {
	s = strings[i];
	if(*s == '\0')
	    continue;		/* empty string */
	while((c = *s++) != '\0')
	    /* skip initial white space */
	    if(!isspace(c))
		break;
	if(c == '\0')
	    continue;		/* only whitespace in string */
	in_white_space = nsep = 0;
	while(c != '\0') {
	    /* look for transitions into/out of white space */
	    if(in_white_space && !isspace(c)){
		++nsep;
		in_white_space = 0;
	    }
	    else if(!in_white_space && isspace(c))
		in_white_space = 1;
	    c = *s++;
	}
	counts[i] = nsep + 1;
    }
    return;
}
