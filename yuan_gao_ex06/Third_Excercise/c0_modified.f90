module sizes

  integer,parameter :: ik=selected_int_kind(10)
  integer,parameter :: ik2=selected_int_kind(14)

  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision

end module sizes


program cc
  use sizes
  implicit none
  real(rk),dimension(:,:),allocatable :: a,b
  real(rk),dimension(:),allocatable :: c
  real(rk) :: x
  integer :: n,i,j,d
  real(rk) :: t0,t1

  n=3000
  d=4

  allocate(a(n,n))
  allocate(b(n,n))
  allocate(c(n))

  do i=1,n
     do j=1,n
        call random_number(x)
        a(i,j)=x
        call random_number(x)
        b(i,j)=x
     end do
     call random_number(x)
     c(i)=x+1.0
  end do

  call cpu_time(t0)
  ! Optimize the loops below.
  do j=1,n
     do i=1,n
        a(i,j)=d*b(i,j)/c(i)  
     end do
  end do
  call cpu_time(t1)

  print *,'c0',t1-t0,sum(a)
end program cc


