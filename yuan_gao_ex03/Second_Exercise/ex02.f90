!--------------------------------------------------!
! From J. Haataja et al., Fortran90/95, CSC, 1998  !
!--------------------------------------------------!

program argh
  implicit none
  integer :: n
  real,allocatable :: b(:),c(:,:),d(:,:),e(:,:)
  real::k(5,5)
  real :: a
  integer :: i,j
  character(len=80) :: argu

  integer :: rs
  integer,allocatable :: rseed(:)

  !----------------------------------------------------!
  ! The following stuff is for initializing the random !
  ! number generator always to the same state.         !
  ! No need to modify this part.                       !
  call random_seed(size=rs)                            !
  allocate(rseed(rs))                                  !
  rseed=123456799                                      !
  call random_seed(put=rseed)                          !
  !----------------------------------------------------!

  call get_command_argument(1,argu)
  read(argu,*) n
  allocate(b(n),c(n,n),d(n,n),e(n,n))

  call random_number(b)
  call random_number(c)

  !  do i=1,n
  !     do j=1,n
  !        d(i,j)=1.0-c(j,i)
  !     end do
  !  end do
  d=1-transpose(c)


  ! do i=2, n
  !    do j=2, n
  !       a=b(j)-b(j-1)
  !       c(i,j)=a*c(i-1,j)+d(i,j)
  !     end do
  !  end do

  !  FORALL(i = 2:n , j = 2:n ) c(i,j)=c(i-1,j)*(b(j)-b(j-1))+d(i,j)
  !  FORALL(i = 2:n) c(i,2:n)=c(i-1,2:n)*(b(2:n)-b(1:n-1))+d(i,2:n)
  !  FORALL(i = 2:n) e = c(1:n,i-1));c(i,2:n)=e*(b(2:n)-b(1:n-1))+d(i,2:n)

  !  c(2:n,2:n)=RESHAPE(   (/(c(i-1,2:n)*(b(2:n)-b(1:n-1))+d(i,2:n),i=2,n)/), (/n-1, n-1/))
  !  c(2:n,2:n)=RESHAPE((/(((b(j)-b(j-1)) *c(i-1,j)+d(i,j)  ,j=2,n),i=2,n)/), (/n-1, n-1/))
  !  c(2:n,2:n)=spread(b(2:n)-b(1:n-1),ncopies = n-1,dim=1) * c(1:n-1,2:n) +d(2:n,2:n)

  !  do i=2,n 
  !     c(i,2:n)=c(i-1,2:n)*(b(2:n)-b(1:n-1))+d(i,2:n)
  !  end do
!  e = c(1,2:n)
!  FORALL(i = 2:n)
!     e = c(i-1,2:n)
!     c(i,2:n)=e*(b/home/gao/Desktop/Tools_For_High_Performance_Computing/yuan_gao_ex03/Second_Exercise(2:n)-b(1:n-1))+d(i,2:n)
!     print *,e
!     e = c(i,2:n)
!     print*,e
!     c(i+1,2:n)=c(i,2:n)*(b(2:n)-b(1:n-1))+d(i+1,2:n)
!  END FORALL
  ! Debug

  e= SPREAD(b - EOSHIFT(b,-1),2,SIZE(c,2))*EOSHIFT(c,-1) + d
  do i=1, n
     do j=1, n
        !print*,c(i,j)
	print*,e(i,j)
     end do
     print *
  end do

  print *
  print *,'answer:', c(n,n)
  print *

end program argh
