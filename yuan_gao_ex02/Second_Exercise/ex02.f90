
!Problem 1. (6 points)
!Write a program which
!a) defines an array to have 100 elements,
!b) assigns to the elements the values 1,2,3,...,100,
!c) reads two integer values in the range of 1..100 (either from stdin or from command line)
!d) reverses the order of the elements of the array in the range specified by the two values.


program iteration
	implicit none
	integer:: n,k,n_o
	k=0  
	print*,"Tell me the initial number n_0:"
	read*,n
	n_o = n
	do while(.true.)  
		k=k+1
		print*,n
		if(mod(n,2)==0) then
			n=n/2
	  	else 
			n=(3*n+1)/2
		end if
		if(n==1 .or. n==2) then
			print*,"after ",k,"iterations, the number ",n_o,"enters cycle."
			exit
		end if
	end do
end program iteration



