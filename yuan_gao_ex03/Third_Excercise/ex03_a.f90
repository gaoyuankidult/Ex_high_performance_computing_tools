module SwapAlgo
  interface swap
     module procedure swap_int,swap_real,swap_string
  end interface swap
contains
  subroutine swap_int(x,y)
    implicit none
    integer :: x,y,temp
    temp=x
    x=y
    y=temp
  end subroutine swap_int
  subroutine swap_real(m,n)
    implicit none
    real :: m,n,temp
    temp=m
    m=n
    n=temp
  end subroutine swap_real
  subroutine swap_string (s1,s2)
    implicit none
    character(len=20) :: s1,s2,temp
    temp=s1
    s1=s2
    s2=temp
  end subroutine swap_string
end module SwapAlgo


program swap_program
  use SwapAlgo
  implicit none
  integer::x,y
  real::m,n
  character(len=20)::s1,s2
  x=1
  y=2
  m=3.0
  n=4.0
  s1="ba"
  s2="ab"
  print*,"Int Before the swap"
  print*,"x= ",x
  print*,"y= ",y
  call swap(x,y)
  print *,"After the swap"
  print *,"x= ",x
  print *,"y= ",y
  print*,"Real Before the swap"
  print*,"m= ",m
  print*,"n= ",n
  call swap(m,n)
  print *,"After the swap"
  print *,"m= ",m
  print*,"n= ",n
  print*,"String Before the swap"
  print*,"s1= ",s1
  print*,"s2= ",s2
  call swap(s1,s2)
  print *,"After the swap"
  print*,"s1= ",s1
  print*,"s2= ",s2

end program swap_program

