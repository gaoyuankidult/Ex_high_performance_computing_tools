module sizes

  integer,parameter :: ik=selected_int_kind(10)
  integer,parameter :: ik2=selected_int_kind(14)

  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision

end module sizes


program bb
  use sizes
  implicit none
  type velocity
     real(rk) :: x
     real(rk) :: y
     real(rk) :: z
  end type velocity
  type(velocity),dimension(:),allocatable :: vel1,vel2
  real(rk) :: x,vx1,vx2,vy1,vy2,vz1,vz2,vd,max_vel_diff
  integer :: n,i,j
  real(rk) :: t0,t1

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

  n=10000

  allocate(vel1(n))
  allocate(vel2(n))

  call random_number(vel1%x)
  call random_number(vel1%y)
  call random_number(vel1%z)
  call random_number(vel2%x)
  call random_number(vel2%y)
  call random_number(vel2%z)

  max_vel_diff=0.0

  call cpu_time(t0)
  ! Optimize the loops below.
  do i=1,n
     vx1=vel1(i)%x
     vy1=vel1(i)%y
     vz1=vel1(i)%z
     do j=1,n
        vx2=vel2(j)%x
        vy2=vel2(j)%y
        vz2=vel2(j)%z
        call diff(vx1,vy1,vz1,vx2,vy2,vz2,vd)
        if(vd>max_vel_diff) then
           max_vel_diff=vd
        end if
     end do
  end do
  call cpu_time(t1)

  print *,'b0',t1-t0,max_vel_diff

end program bb


subroutine diff(vx1,vy1,vz1,vx2,vy2,vz2,vd)
  use sizes
  implicit none
  real(rk) :: vd,vx1,vx2,vy1,vy2,vz1,vz2

  vd=sqrt((vx2-vx1)**2+(vy2-vy1)**2+(vz2-vz1)**2)

end subroutine diff
