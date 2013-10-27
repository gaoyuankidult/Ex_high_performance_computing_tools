
program my_int2str
  implicit none
  integer :: k
  character(80) :: str
  k=12345
  str = int2str(k)
  print *,"original integer is", k
  write(*,'(a,a)')  'the output of conversion = ', str  
contains
  character(80) function int2str(num) 
     integer, intent(in):: num
     character(20) :: str
     ! convert integer to string using formatted write
     write(str, '(i20)') num
     int2str = adjustl(str)
  end function int2str
end program my_int2str 




