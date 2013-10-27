!Study what kind of integer and real types the compiler you use supports. Use the
!program given below.

!program testkind
!	implicit none
!	integer :: i,ki,kr
!	do i=1,24
!		ki=selected_int_kind(i)
!		kr=selected_real_kind(i)
!	print *,i,ki,kr
!	end do
!end program testkind

!If possible run the program in more than one environment.


program testkind
  implicit none
  integer :: i,ki,kr
  do i=1,24
     ki=selected_int_kind(i)
     kr=selected_real_kind(i)
     print *,i,ki,kr
  end do
end program testkind

!Answer:
!           1           1           4
!           2           1           4
!           3           2           4
!           4           2           4
!           5           4           4
!           6           4           4
!           7           4           8
!           8           4           8
!           9           4           8
!          10           8           8
!          11           8           8
!          12           8           8
!          13           8           8
!          14           8           8
!          15           8           8
!          16           8          10
!          17           8          10
!          18           8          10
!          19          -1          16
!          20          -1          16
!          21          -1          16
!          22          -1          16
!          23          -1          16
!          24          -1          16


