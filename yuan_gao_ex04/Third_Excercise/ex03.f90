program randomnumbers
  implicit none
  integer,parameter :: rk=selected_real_kind(10,40)
  real(rk),pointer :: r(:,:),max_val(:),max_loc_raw(:)
  integer,pointer :: max_loc(:),statistics(:)
  integer :: s,i
  integer :: lines
  integer,allocatable :: seed(:)
  lines = 10
  call random_seed(size=s)     ! Find out the seed array size
  allocate(seed(s))            ! Allocate space for seed array
  seed=976345                  ! Assign value seeds (well, same value for all)
  call random_seed(put=seed)   ! Set the RNG seed
  allocate(r(10,lines))
  allocate(max_loc(lines))
  allocate(max_val(lines))
  allocate(max_loc_raw(lines))
  allocate(statistics(lines))
  do i = 1,lines
     call random_number(r(:,i))
     max_loc_raw = maxloc(r(:,i))
     max_loc(i) = int(max_loc_raw(1))
     max_val(i) = maxval(r(:,i))
  end do
  print *,"The maximum value of 10 sequences of length 10 are:"
  do i=1,lines
     print *,max_val(i)
  end do
  print *
  print *,"The distribution of position of the maximum values are as follows:"
  do i = 1 ,lines
     statistics(max_loc(i)) =   statistics(max_loc(i)) + 1
  end do
  do i=1,lines
     print *,"maximum apeared at position",i,statistics(i),"times"
  end do
  stop

end program randomnumbers
