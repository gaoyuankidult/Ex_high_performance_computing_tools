program vectorization
  integer:: len = 40000000
  real,dimension(40000000) ::a,b, c, x1, x2
  real :: s,t1,t2
  call random_number(a)
  call random_number(b)
  call random_number(c)
  call random_number(x1)
  call random_number(x2)

  call cpu_time(t1)
  do i=1,len 
     s = b(i)**2 - 4.*a(i)*c(i) 
        x1(i) = sqrt(s) 
        x2(i) = (-x1(i) - b(i)) *0.5 / a(i) 
        x1(i) = ( x1(i) - b(i)) *0.5 / a(i) 
  enddo
  call cpu_time(t2)
  print*,t2-t1

end program vectorization
