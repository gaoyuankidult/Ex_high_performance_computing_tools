program array_order_test
  implicit none
  integer,parameter :: rk=selected_real_kind(10,40),N=10000
  integer :: a(N,N)
  integer :: i,j
  real :: t1,t2,t3,t4
  a=0


  call cpu_time(t1)
  do i=1,N
     do j=1,N
        a(i,j)=1
     end do
  end do
  call cpu_time(t2)



  call cpu_time(t3)
  do j=1,N
     do i=1,N
        a(i,j)=1
     end do
  end do
  call cpu_time(t4)
  print*,"CPU Time That Does Not Obey Storage Structure of System (Row-majored):"
  print*,(t2-t1)
  print*,"CPU Time That Obeys Storage Structure of System(Column-majored) :"
  print*,(t4-t3)
  stop
end program array_order_test
