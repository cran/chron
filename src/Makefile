LIBNAME=chron
LD=ld

OBJS=   chron_strs.o

$(LIBNAME).so: $(OBJS)
	@$(LD) $(SHLIBLDFLAGS) -o $(LIBNAME).so $(OBJS)

clean:
	@rm -f *.o *.so
