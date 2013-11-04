program array_order_test
  implicit none
  real :: t1,t2,t3,t4,sum_value 
  integer::  i,j,k
  integer,parameter :: rk=selected_real_kind(10,40)   
  integer,parameter:: N = 1000
  type coord 
     real(rk)::x 
     real(rk)::y 
     real(rk)::z 
  end type coord
  type(coord),allocatable::r1(:)

  type coordarr
     real(rk),allocatable::x(:)
     real(rk),allocatable::y(:)
     real(rk),allocatable::z(:)
  end type coordarr
  type(coordarr)::r2
  allocate(r1(N))
  allocate(r2%x(N))
  allocate(r2%y(N))
  allocate(r2%z(N))
  r1%x=1
  r1%y=1
  r1%z=1
  r2%x=1
  r2%y=1
  r2%z=1

  sum_value = 0
  call cpu_time(t1)
  do i = 1,N
     do j = 1, N
        if(i /= j) then 
           sum_value = sqrt((r1(i)%x-r1(j)%x)**2+(r1(i)%x-r1(j)%x)**2+(r1(i)%x-r1(j)%x)**2) + sum_value
        end if
     end do
  end do
  call cpu_time(t2)
  print*,sum_value
  sum_value = 0


  call cpu_time(t3)
  do i = 1,N
     do j = 1, N
        if(i /= j) then 
           sum_value = sqrt((r2%x(i)-r2%x(j))**2 +(r2%y(i)-r2%y(j))**2 +(r2%z(i)-r2%z(j))**2) +sum_value
        end if
     end do
  end do
  print*,sum_value
  call cpu_time(t4)
  print*,"CPU Time for AOS"
  print*,(t2-t1)
  print*,"CPU Time for SOA"
  print*,(t4-t3)
  stop
end program array_order_test
