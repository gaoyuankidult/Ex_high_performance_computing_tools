module sizes

  integer,parameter :: ik=selected_int_kind(10)
  integer,parameter :: ik2=selected_int_kind(14)

  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision

end module sizes


program ee
  use sizes
  implicit none
  real(rk),dimension(:,:),allocatable :: a,b
  real(rk) :: x,v
  integer :: n,i,j
  real(rk) :: t0,t1

  n=3000
  v=2

  allocate(a(n,n))
  allocate(b(n,n))

  call random_number(a)
  call random_number(b)

  call cpu_time(t0)
  ! Optimize the loops below.
  do j=1,n
     do i=2,n
        a(i,j)=a(i-1,j)/v+ i/j
        b(i,j)=b(i-1,j)/v+ i/j
     end do
  end do
  call cpu_time(t1)

  print *,'e0',t1-t0,sum(a)+sum(b)
end program ee


