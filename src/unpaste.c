#define BUF_SIZ 4096
void
unpaste(strings, nstrings, sep, white_space, n_fields, output)
	char	**strings, **sep;
	long	*nstrings, *white_space, *n_fields;
	vector	**output;		/* S list */
{
	long	n = *nstrings, whitespace = *white_space, nfields = *n_fields;
	long	i, j, k;
	char	c_sep = **sep, *s, **to_char, buffer[BUF_SIZ];
	int	c;
/* allocate character vectors for each field */
for(k = 0; k < nfields; ++k)
	output[k] = alcvec(CHAR, n);

/* the delimiter may be a one-char separator or whitespace */
if(!whitespace){
	for(i = 0; i < n; ++i){
		s = strings[i];
		j = k = 0;
		while(1){
			c = *s;
			if( c==c_sep || c=='\0') {
				buffer[j++] = '\0';
				to_char = output[k++]->value.Char;
				to_char[i] = Calloc(j, char);
				(void) strcpy(to_char[i], buffer);
				if(c=='\0') break;
				j = 0;
			}
			else	buffer[j++] = c;
			++s;
		}
	}
return;
}
/* items are separted by whitespace */
for(i = 0; i < n; ++i){
	s = strings[i];
	while( isspace(*s) ) ++s;	/* skip initial whitespace */
	j = k = 0;
	while(1){
		c = *s;
		if(isspace(c) || c=='\0'){
			buffer[j++] = '\0';
			to_char = output[k++]->value.Char;
			to_char[i] = Calloc(j, char);
			(void) strcpy(to_char[i], buffer);
			if(c=='\0') break;
			j = 0;
			/* skip trailing space in current item */
			while(isspace(*(s+1))) ++s;
			if(*(s+1)=='\0') break;
		} 
		else	buffer[j++] = c;
		++s;
	}
}
return;
}
