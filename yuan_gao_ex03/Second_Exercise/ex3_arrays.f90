!--------------------------------------------------!
! From J. Haataja et al., Fortran90/95, CSC, 1998  !
!--------------------------------------------------!

program argh
  implicit none
  integer :: n
  real,allocatable :: b(:),c(:,:),d(:,:)
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
  allocate(b(n),c(n,n),d(n,n))

  call random_number(b)
  call random_number(c)

  do i=1,n
     do j=1,n
        d(i,j)=1.0-c(j,i)
     end do
  end do

  do i=2, n
     do j=2, n
        a=b(j)-b(j-1)
	print *,a*c(i-1,j)+d(i,j)
        c(i,j)=a*c(i-1,j)+d(i,j)
     end do
  end do

  print *
  print *,'answer:', c(n,n)
  print *

end program argh
