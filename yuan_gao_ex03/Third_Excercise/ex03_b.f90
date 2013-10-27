module SwapAlgo
  interface swap
     module procedure swap_int,swap_real,swap_string,swap_integer_array,swap_integer_matrix,swap_real_array,swap_real_matrix
  end interface swap
contains
  subroutine swap_real_matrix(p,q)
    implicit none
    integer::i,j
    real,dimension(:,:)::p,q
    i=1
    do while(i .le. size(p,dim=1))
       call swap_real_array(p(i,:),q(i,:))
       i = i + 1
    end do
  end subroutine swap_real_matrix
  subroutine swap_real_array(a,b)
    implicit none
    integer::i,j
    real,dimension(:)::a,b
    i=0
    do while(i .le. size(a))
       call swap_real(a(i),b(i))
       i=i+1
    end do
  end subroutine swap_real_array

  subroutine swap_integer_matrix(p,q)
    implicit none
    integer::i,j
    integer,dimension(:,:)::p,q
    i=1
    do while(i .le. size(p,dim=1))
       call swap_integer_array(p(i,:),q(i,:))
       i = i + 1
    end do
  end subroutine swap_integer_matrix
  subroutine swap_integer_array(a,b)
    implicit none
    integer::i,j
    integer,dimension(:)::a,b
    i=0
    do while(i .le. size(a))
       call swap_int(a(i),b(i))
       i=i+1
    end do
  end subroutine swap_integer_array
  subroutine swap_int(x,y)
    implicit none
    integer :: x,y,temp
    temp=x
    x=y
    y=temp
  end subroutine swap_int
  subroutine swap_real(m,n)
    implicit none
    real :: m,n,temp
    temp=m
    m=n
    n=temp
  end subroutine swap_real
  subroutine swap_string (s1,s2)
    implicit none
    character(len=20) :: s1,s2,temp
    temp=s1
    s1=s2
    s2=temp
  end subroutine swap_string
end module SwapAlgo


program swap_program
  use SwapAlgo
  implicit none
  real,dimension(5)::a,b
  real,dimension(2,3)::p,q
  a(:)= 8.0
  b(:)= 10.0
  print *,"##a and b are both one dimensional array##"
  print *,"Before a and b are swapped"          
  print *,"a=",a                                                                                         
  print *,"b=",b                                                                                         
  call swap(a,b)                                                                                         
  print *,"After a and b are swapped"                                                                     
  print *,"a=",a                                                                                         
  print *,"b=",b
  
  p(:,:) = 3.0
  q(:,:) = 4.0
  print *,"##p and q are both two dimensional array##"
  print *,"Before p and q are swapped"
  print *,"p=",p
  print *,"q=",q
  call swap(p,q)
  print *,"After p and q are swapped"
  print *,"p=",p                                                                                         
  print *,"q=",q 

end program swap_program
