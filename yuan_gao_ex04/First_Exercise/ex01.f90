program stackoverflow
  integer a(8)
  a = (/ 1,2,3,4,5,6,7,8 /)
  do i=1,10
     print*,a(i)
  end do
end program stackoverflow

! The result obtained without parameter -fcheck=bounds
!gfortran ex01.f90 && ./a.out
!           1
!           2
!           3
!           4
!           5
!           6
!           7
!           8
!           0
!           0

! The result obtained with parameter -fcheck=bounds
!gfortran ex01.f90 -fcheck=bounds && ./a.out
!           1
!           2
!           3
!           4
!           5
!           6
!           7
!           8
!At line 5 of file ex01.f90
!Fortran runtime error: Index '9' of dimension 1 of array 'a' above upper bound of 8







