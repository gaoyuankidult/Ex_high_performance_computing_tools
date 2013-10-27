
!Problem 1. (6 points)
!Write a program which
!a) defines an array to have 100 elements,
!b) assigns to the elements the values 1,2,3,...,100,
!c) reads two integer values in the range of 1..100 (either from stdin or from command line)
!d) reverses the order of the elements of the array in the range specified by the two values.


program define_arrays
  implicit none
  integer:: k,i,j,temp,read_integer1,read_integer2
  !a) define anarray to have 100 elements
  integer array(100)
  !b) assigns to the elements the values 1,2,3,...,100,
  array = (/(i,i=1,100)/)
  !c) reads two integer values in the range of 1..100 (either from stdin or from command line)
  do
     read (*,*) read_integer1,read_integer2
     if (read_integer1 .LE. 100 .and. read_integer1 .GE.0 .and. read_integer2.GE.0 .and. read_integer2.LE.100) exit

  end do
  !d) reverses the order of the elements of the array in the range specified by the two values.
  do k = read_integer1,(read_integer2-read_integer1)/2	
	temp = array(k)
	array(k)=array(read_integer2-k+read_integer1)
	array(read_integer2-k+read_integer1)=temp
  end do
end program define_arrays



