program randomnumbers
  implicit none
  integer,parameter :: rk=selected_real_kind(10,40)
  real(rk) :: r(10)
  integer :: s
  integer,allocatable :: seed(:)

  call random_seed(size=s)     ! Find out the seed array size
  allocate(seed(s))            ! Allocate space for seed array
  seed=976345                  ! Assign value seeds (well, same value for all)
  call random_seed(put=seed)   ! Set the RNG seed
  call random_number(r)        ! Finally, fill array r with RN's 
  print '(100f10.4)',r

  stop

end program randomnumbers
