program forallexample
  implicit none
  integer :: i,j

  integer:: n
  real :: t1,t2,t3,t4
  real*4,pointer,dimension(:)  ::real4
  real*4 :: sum4
  real*8, pointer,dimension(:)   ::real8
  real*8 :: sum8
  real*16, pointer,dimension(:) ::real16
  real*16 :: sum16
  open (unit = 2, file = "data_signal_&_double.dat")

  do n= 0,100000,2000

     sum4 = 0
     sum8 = 0
     sum16 = 0


     allocate(real4(n))
     allocate(real8(n))
     allocate(real16(n))
     real4 = 19.0
     real8 = 19.0
     real16 = 19.0   
     call cpu_time(t1)
     do i=1,n-1
        do j= i+1,n
           sum4 = sum4 + (real4(j) -real4(i))
        end do
     end do
     call cpu_time(t2)
     do i=1,n-1
        do j= i+1,n
           sum8 = sum8 + (real8(j) -real8(i))
        end do
     end do
     call cpu_time(t3)
!     do i=1,n-1
!        do j= i+1,n
!           sum16 = sum16 + (real16(j) -real16(i))
!        end do
!     end do
     call cpu_time(t4)
     print*,"CPU Time for Summing up Singal Precision Real",(t2-t1)
     print*,"CPU Time for Summing up Double Precision Real",(t3-t2)
!     print*,"CPU Time for Summing up Quadruple Precision Real",(t4-t3)
      write(2,*),n
     write (2,*),(t2-t1)
     write (2,*),(t3-t2)
 !    write (2,*),(t4-t3)
     deallocate(real4)
     deallocate(real8)
     deallocate(real16)  
  end do
  close(5)
end program forallexample

