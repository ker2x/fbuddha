CC = gcc
CFLAGS = -c -O2 -Wall -D_GCC
AR = ar
AFLAGS = -sr

ALL : libjapi.a 

# Object Files
japi4f.o : japi4f.c
	$(CC) $(CFLAGS) japi4f.c
japi4c.o : japi4c.c
	$(CC) $(CFLAGS) japi4c.c
imageio.o : imageio.c
	$(CC) $(CFLAGS) imageio.c
fileselect.o : fileselect.c
	$(CC) $(CFLAGS) fileselect.c
japilib.o : japilib.c japi_p.h japiserver.h  japi.h 
	$(CC) $(CFLAGS) japilib.c 


# link together 
libjapi.a  : japilib.o japi4c.o japi4f.o imageio.o fileselect.o 
	$(AR) $(AFLAGS) $@ japilib.o japi4c.o japi4f.o imageio.o fileselect.o 

# cleanman
clean :
	rm *.a *.o
