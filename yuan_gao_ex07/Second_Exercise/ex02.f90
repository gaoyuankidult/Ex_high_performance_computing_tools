program array_order_test
  implicit none
  integer,parameter :: n=1000
  real :: a(n,n),b(n,n),c(n,n)
  integer :: i,j,k
  real :: t1,t2,t3,t4
  call random_number(a)
  call random_number(b)
  call cpu_time(t1)
  c = 0
  do j=1, n
     do i=1, n
        do k=1, n
           c(i,j)=c(i,j)+a(i,k)*b(k,j)
        enddo
     enddo
  enddo
  call cpu_time(t2)

  c = 0

  call cpu_time(t3)
  a = transpose(a)
  do j=1, n
     do i=1, n
        do k=1, n
           c(i,j)=c(i,j)+a(i,k)*b(k,j)
        enddo
     enddo
  enddo
  call cpu_time(t4)
  print*,"Time of C = A * B"
  print*,(t2-t1)
  print*,"Time of C = A^t * B"
  print*,(t4-t3)
  stop
end program array_order_test
