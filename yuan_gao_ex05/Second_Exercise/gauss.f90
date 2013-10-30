module sizes
  integer,parameter :: rk=selected_real_kind(10,40)
  integer,parameter :: MATSIZE=4
end module sizes

subroutine gauss(a,b,x)
  use sizes
  implicit none
  real (rk), intent(inout) :: a(MATSIZE,MATSIZE),b(MATSIZE)
  real (rk), intent(out) :: x(MATSIZE)
  integer :: p(MATSIZE)
  real(rk) :: s(MATSIZE)
  integer :: i,j,k,pk
  real(rk) :: smax,rmax,r,z,ss

  do i=1,MATSIZE
     p(i) = i
     smax = 0.0 
     do j=1,MATSIZE
        smax = max(smax,abs(a(i,j)))
     enddo
     s(i) = smax
  enddo
  
  do k=1,MATSIZE-1
     rmax = 0.0
     do i=k,MATSIZE
        r = abs(a(p(i),k))/s(p(i))
        if (r > rmax) then
           j = i
           rmax = r
        endif
     enddo
     
     pk = p(j)
     p(j) = p(k)
     p(k) = pk
     
     do i=k+1,MATSIZE      
        z = a(p(i),k)/a(p(k),k)       
        a(p(i),k) = z
        do j=k+1,MATSIZE    
           a(p(i),j) = a(p(i),j) - z*a(p(k),j)      
        enddo
     enddo
  enddo

  do k=1,MATSIZE-1
     do i=k+1,MATSIZE
        b(p(i)) = b(p(i)) - a(p(i),k)*b(p(k))
     enddo
  enddo

  backsubst: do i=MATSIZE,1,-1
     ss = b(p(i))
     do j=i+1,MATSIZE
        ss = ss - a(p(i),j)*x(j)
     enddo
     x(i) = ss/a(p(i),i)
  enddo backsubst
  
  return
end subroutine gauss


program gaussmain
  use sizes
  implicit none
  real(rk) :: a(MATSIZE,MATSIZE),b(MATSIZE),x(MATSIZE)
  integer :: i,j

  do i=1,MATSIZE
     read(5,*) (a(i,j),j=1,MATSIZE)
  enddo
  read(5,*) (b(i),i=1,MATSIZE)

  write(6,'(/a)') 'Before'
  write(6,'(/a)') 'A'
  do i=1,MATSIZE
     write(6,'(20g16.8)') (a(i,j),j=1,MATSIZE)
  enddo
  write(6,'(/a)') 'b'
  write(6,'(20g16.8)') b

  call gauss(a,b,x)
  
  write(6,'(/a)') 'After'
  write(6,'(/a)') 'A'
  do i=1,MATSIZE
     write(6,'(20g16.8)') (a(i,j),j=1,MATSIZE)
  enddo
  write(6,'(/a)') 'x'
  write(6,'(20g16.8)') x

end program gaussmain
