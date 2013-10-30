module ackermann
implicit none
integer:: indicator = 0
integer:: max = -1
integer:: monitor
contains
  recursive function ack(m, n) result(a)
    integer, intent(in) :: m,n
    integer :: a
    indicator = indicator +1
    if (max <= indicator) then
       max = indicator
    end if
    if(monitor == 1) then
       print*,"endtered to next depth :",indicator
    end if

    if (m == 0) then
       a=n+1
    else if (n == 0) then
       a=ack(m-1,1)
    else
       a=ack(m-1, ack(m, n-1))
    end if
    indicator = indicator -1
    if(monitor == 1) then
       print*,"return to previous depth",indicator
    end if
  end function ack
end module ackermann

program ackermann_prog
  use ackermann
  implicit none
  integer::ack_value
  print*,"Do you want to monitor the process ? (1 for yes, else for no)" 
  read(*,*),monitor
  ack_value = ack(3, 7) 
  print*,"The answer of ackermann function is :", ack_value
  print*,"The maximum depth of recursion is   :", max
end program ackermann_prog
