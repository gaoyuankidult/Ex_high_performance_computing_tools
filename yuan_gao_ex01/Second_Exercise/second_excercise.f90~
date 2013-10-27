!Find all errors in the following Fortran program:
!
!progarm buggy
!	implicit none
!	integer :: i
!	real :: pi=3*arctan(1.0)
!	print *,'pi = ' pi
!	i=4
!	j=10.0-i
!	print *,"i/(10-i) =',i/j
!end program buggy


!Answer:
program buggy
	implicit none
	integer :: i
	real :: j,pi=2*asin(1.0)
	print *,'pi = ', pi
	i=4
	j=10.0-i
	print *,"i/(10-i) =",i/j
end program buggy

