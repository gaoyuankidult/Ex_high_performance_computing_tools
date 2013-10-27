program to_matrix
  implicit none 
  real, pointer :: a(:)
  real, pointer::b(:,:)
  integer::i,size_of_a,start
  integer :: n = 12
  allocate(a(n))
  a = (/1,1,1,1,1,1,1,1,1,1,1,1/)
  size_of_a = size(a)
  start=sqrt(real(size_of_a))
  do i = start,size_of_a
     if (mod(size_of_a,i)  == 0) then
        allocate(b(i,size_of_a/i))
        b=reshape(a,(/i,size_of_a/i/))
        exit
     end if
  end do
  print*,"In this example, we input an array a of size 12 and the output is b."
  print*,"The shape of b is:"
  print*, shape(b)
  deallocate(a)
  deallocate(b)
end program to_matrix
