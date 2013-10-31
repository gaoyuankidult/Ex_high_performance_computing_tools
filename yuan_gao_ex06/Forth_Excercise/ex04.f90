program array_order_test
  implicit none
  real :: t1,t2,t3,t4

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
  type(coordarr)::r2)


  call cpu_time(t1)

  call cpu_time(t2)



  call cpu_time(t3)

  call cpu_time(t4)
  print*,"CPU Time for SOA"
  print*,(t2-t1)
  print*,"CPU Time for AOS"
  print*,(t4-t3)
  stop
end program array_order_test
