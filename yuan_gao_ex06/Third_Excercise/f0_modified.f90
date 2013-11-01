module sizes

  integer,parameter :: ik=selected_int_kind(10)
  integer,parameter :: ik2=selected_int_kind(14)

  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision

end module sizes


program ff
  use sizes
  implicit none
  integer,dimension(:,:),allocatable :: a,b
  integer :: n,i,j,s
  real:: t0,t1

  n=3000

  allocate(a(n,n))
  allocate(b(n,n))

  do j=1,n
     do i=1,n
        a(i,j)=(i-1)*n+j
     end do
  end do

  call cpu_time(t0)
  ! Optimize the loops below.
  do i=1,n,2
     do j=1,n,2
        b(i,j)=a(j,i) 
        b(i+1,j)=a(j,i+1)
        b(i,j+1)=a(j+1,i)
        b(i+1,j+1)=a(j+1,i+1)
     end do
  end do
!  b = transpose(a)
  call cpu_time(t1)

  s=sum(b,mask=b<5)

  print *,'f0',t1-t0,s
end program ff


