program matrix_inversion
	implicit none
	integer::i,j
	real A(5,4)
	real B(4,3)
	do i = 1, 5
		do j = 1, 4
			A(i,j) = 10 * i + j
    		end do
  	end do
	do i = 1, 4
		do j = 1, 2
			B(i,j) = 10 * i + j
    		end do
  	end do
	print*,"AXB"
	print*,matmul(A,B)
	print*,"B_tXA_t"
	print*,matmul(transpose(B),transpose(A))
end program matrix_inversion



