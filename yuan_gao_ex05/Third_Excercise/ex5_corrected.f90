program rdist

  !----------------------------------------------------------------!
  ! Program is supposed to calculate and print the distribution of !
  ! uniform random numbers.                                        !
  !----------------------------------------------------------------!
  !integer,parameter :: jmin=1,jmax=1000 
  implicit none


  integer,parameter :: jmin=1,jmax=100
  integer, parameter :: double=selected_real_kind(p=15,r=100)
  real(double),parameter :: bscale=100.0

  integer :: bin(jmin:jmax),i,n,j
  integer :: r
  real(double) :: x

  real(double),external :: rand1

  bin(jmin:jmax) = 0

  n=1000000
  do i=1,n
     x=rand1()     
     j=floor(bscale*x+1) 
     !j=floor(bscale*x+0.5) 
     bin(j)=bin(j)+1
  end do
  open (unit = 2, file = "data.dat")
  do r=jmin,jmax
     !print *,r/bscale,bin(r)
     write(2,*),r
     write(2,*),bin(r)
     print *,r,bin(r)
  end do
  close(2)

end program rdist


!----------------------------------------------------------------!


function rand1()

  !---------------------------------------!
  ! This routine is ok. No need to debug. !
  !---------------------------------------!
  !    random number generator       !
  !    uniform distribution [0,1[    !
  !    ix = seed < jj                !
  !----------------------------------!

  integer, parameter :: double=selected_real_kind(p=15,r=100)
  real(double) :: rand1
  logical, save :: first_time=.true.
  integer, save :: ix   
  integer :: seed=2342345
  integer :: k1
  integer,parameter :: ii=127773,jj=2147483647,i1=16807,i2=2836
  real(double),parameter :: p=4.65661287d-10
  character(len=10) :: a
  character(len=5) :: c
  integer, dimension(8) :: val

  if (first_time) then
     call date_and_time(a,a,c,val)
     seed=val(8)
     ix=seed
     first_time=.false.
  end if

  k1 = ix/ii
  ix = i1*( ix - k1*ii) - k1 * i2
  if ( ix .lt. 0) ix = ix + jj
  rand1 = ix * p

  return
end function rand1

