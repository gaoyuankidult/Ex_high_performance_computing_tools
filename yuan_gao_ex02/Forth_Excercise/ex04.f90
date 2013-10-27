!Write a Fortran program that fills two arrays a and b with random numbers [intrinsic routine random_number()] and zeroes all elements in b for which the corresponding element in a is smaller than 0.5. You can use either a do loop or the where construct. Array size and random number generator seed are given as input from the user. Random number generator seed can be set by routine random_seed() (see the example program randomtest.f90 on the course web page).

program random_number
	implicit none
	integer:: i,j = 10,k1
	integer,allocatable :: seed(:)
	real :: a(10),b(10)
	print*,"Input the size:"
	read*,k1
	call random_seed(size = k1)
	allocate(seed(k1))
	print*,"Input the seed:"
	read*,seed
	call random_seed(put=seed)
	call random_number(a)
	call random_number(b)
	do i = 1, j
		if(a(i) < 0.5) then
			 b(i)=0
		end if
	end do
	print*,"a:"
	print*,a
	print*,"b:"
	print*,b
end program random_number



