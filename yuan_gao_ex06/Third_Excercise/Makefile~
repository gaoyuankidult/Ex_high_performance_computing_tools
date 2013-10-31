GF=gfortran
FFLAGS=-c -Wall
LDFLAGS=
SOURCES=a0.f90 a0_modified.f90 b0.f90 b0_modified.f90 c0.f90 c0_modified.f90 d0.f90 d0_modified.f90 e0.f90 e0_modified.f90 f0.f90 f0_modified.f90
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=hello

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@
