module sizes

  integer,parameter :: ik=selected_int_kind(10)
  integer,parameter :: ik2=selected_int_kind(14)

  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision

end module sizes


program dd
  use sizes
  implicit none
  real(rk),dimension(:),allocatable :: c,b
  real(rk) :: x,s
  integer :: n,n2,i,j
  real(rk) :: t0,t1

  n=150000
  n2=1500

  allocate(b(n2))
  allocate(c(n+n2))

  do i=1,n2
     call random_number(x)
     b(i)=10.0*x
  end do

  do i=1,n+n2
     call random_number(x)
     c(i)=10.0*x
  end do

  call cpu_time(t0)
  ! Optimize the loops below.
  do i=1,n
     do j=1,n2
        c(i)=c(i)+c(i+j-1)*b(j)
     end do
  end do
  call cpu_time(t1)

  print *,'d0',t1-t0,sum(c)
end program dd


