module sizes

  integer,parameter :: ik=selected_int_kind(10)
  integer,parameter :: ik2=selected_int_kind(14)

  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision

end module sizes


program aa
  use sizes
  implicit none
  integer,dimension(:),allocatable :: a,b
  real(rk) :: x
  integer :: n,i
  real(rk) :: t0,t1
    
  n=10000000
  
  allocate(a(n))
  allocate(b(n+1))
  
  do i=1,n
     call random_number(x)
     a(i)=int(10*x)
     call random_number(x)
     b(i)=int(10*x)
  end do
  
  b(n+1)=100
  
  call cpu_time(t0)
  ! Optimize the loop below.
  do i=1,n
     if(i<500) then
        a(i)=4.0*b(i)+b(i+1)  
     else
        a(i)=4.0*b(i+1)+b(i)
     end if
  end do
  call cpu_time(t1)
  
  print *,'a0',t1-t0,sum(a)
end program aa


