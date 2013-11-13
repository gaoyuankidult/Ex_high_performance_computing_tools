program array_order_test
  implicit none
  integer,parameter :: n=1000
  real :: a(n,n),b(n,n),c(n,n)
  integer :: i,j,k
  real :: t1,t2,t3,t4
  call random_number(a)
  call random_number(b)
  c = 0
  call cpu_time(t1)
  c=matmul(a,b)

  call cpu_time(t2)

  c = 0

  call cpu_time(t3)
  a = transpose(a)
  !c=matmul(transpose(a),b)
  c=matmul(a,b)
  call cpu_time(t4)
  print*,"Time of C = A * B"
  print*,(t2-t1)
  print*,"Time of C = A^t * B"
  print*,(t4-t3)
  stop
end program array_order_test
