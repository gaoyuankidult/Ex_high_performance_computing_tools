program matrix_multi_compare
  real, pointer,dimension(:,:) :: randarray1
  real, pointer,dimension(:,:) :: randarray2
  real,pointer,dimension(:,:):: randarray3
  real :: t1,t2,rate
  integer :: c1,c2,cr,cm,i,j,n,s,p
  integer , parameter :: x=20000,y=15000,runs=20
  real :: array(x,y),a_diff,diff
  open (unit = 2, file = "data.dat")
  do p=800,800
     print*,p,"of 20 to 800"
     allocate(randarray1(p,p))
     allocate(randarray2(p,p))
     allocate(randarray3(p,p))
     call random_number(randarray1)
     call random_number(randarray2)
     ! First initialize the system_clock
     call system_clock(count_rate=cr)
     call system_clock(count_max=cm)
     rate = real(cr)
     write(*,*) "system_clock rate ",rate

     print*,"System record for matmul matrix multiplication:"
     diff = 0.0
     a_diff = 0.0
     s = 0
     do n = 1 , runs
        call CPU_TIME(t1)
        call SYSTEM_CLOCK(c1)
        randarray3=matmul(randarray2,randarray1)
        call CPU_TIME(t2)
        call SYSTEM_CLOCK(c2)
        array(1,1) = array(1,2)     
        if ( (c2 - c1)/rate < (t2-t1) ) s = s + 1
        diff = (c2 - c1)/rate - (t2-t1) + diff
        a_diff = ABS((c2 - c1)/rate - (t2-t1)) + a_diff
     end do

     write(*,*) "system_clock : ",(c2 - c1)/rate
     write(*,*) "cpu_time     : ",(t2-t1)
     write(*,*) "sc < ct      : ",s,"of",runs
     write(*,*) "mean diff    : ",diff/runs
     write(*,*) "abs mean diff: ",a_diff/runs
     write(2,*),p
     write (2,*),(t2-t1)
     print *
     print*,"System record for do loop matrix multiplication:"
     diff = 0.0
     a_diff = 0.0
     s = 0
     do n = 1 , runs
        call CPU_TIME(t1)
        call SYSTEM_CLOCK(c1)
        do j=1, p
           do i=1, p
              do k=1, p
                 randarray3(i,j)=randarray3(i,j)+randarray2(i,k)*randarray1(k,j)
              enddo
           enddo
        enddo
        call CPU_TIME(t2)
        call SYSTEM_CLOCK(c2)
        array(1,1) = array(1,2)     
        if ( (c2 - c1)/rate < (t2-t1) ) s = s + 1
        diff = (c2 - c1)/rate - (t2-t1) + diff
        a_diff = ABS((c2 - c1)/rate - (t2-t1)) + a_diff
     end do

     write(*,*) "system_clock : ",(c2 - c1)/rate
     write(*,*) "cpu_time     : ",(t2-t1)
     write(*,*) "sc < ct      : ",s,"of",runs
     write(*,*) "mean diff    : ",diff/runs
     write(*,*) "abs mean diff: ",a_diff/runs
     write (2,*),(t2-t1)
     deallocate(randarray1)
     deallocate(randarray2)
     deallocate(randarray3)
  end do
  close(5)
end program matrix_multi_compare

