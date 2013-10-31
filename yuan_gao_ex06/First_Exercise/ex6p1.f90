program forallexample
  implicit none
  integer,parameter :: rk=selected_real_kind(10,40),N=10000
  integer :: a(N,N)
  integer :: i,j
  real :: rate,t2,t1

  a=0
  call cpu_time(t1)
  forall (i=1:N:2,j=1:N:2,i+j>10)
     a(i,j)=(i+j)/2
  end forall
  call cpu_time(t2)
  !print *
  !print '(10i8)',a(1:N:1000,1:N:1000)
  !print *
  print*,"CPU Time",(t2-t1)
  stop
end program forallexample
