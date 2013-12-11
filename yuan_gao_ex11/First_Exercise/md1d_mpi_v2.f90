!------------------------------------------------------------------
! 1D anharmonic system, MPI version
! A.Kuronen, 2009
!------------------------------------------------------------------

module sizes
  integer,parameter :: rk0=selected_real_kind(5,10)    ! single precision
  integer,parameter :: rk=selected_real_kind(10,40)    ! double precision
  integer,parameter :: rk2=selected_real_kind(30,200)  ! quadruple precision
  integer,parameter :: MAXBUF=200
end module sizes


program md1d
  use sizes
  use mpi
  implicit none  
  interface
     integer function getseed(info,file)
       integer,intent(in) :: info,file
     end function getseed
  end interface

  !----------------------------------------------------------------
  ! Simulation system variables
  !----------------------------------------------------------------

  real(rk),allocatable :: x(:)   ! atom positions
  real(rk),allocatable :: v(:)   !    velocities
  real(rk),allocatable :: v0(:)  !    previous veloocities (leap frog needs them)
  real(rk),allocatable :: a(:)   !    accelerations
  real(rk),allocatable :: ep(:)  !    potential energies
  real(rk),allocatable :: ek(:)  !    kinetic energies

  real(rk) :: epsum,eksum        ! system energies

  real(rk) :: dt                 ! time step 
  real(rk) :: vsc                ! mean initial velocity
  real(rk) :: box                ! system size
  integer :: nat                 ! number of atoms
  integer :: maxt                ! number of time steps simulated
  integer :: eout                ! energy output interval
  integer :: cout                ! coordinate output interval (lot of data, beware!)

  !----------------------------------------------------------------
  ! Misc variables
  !----------------------------------------------------------------

  integer :: i,n,ia
  character(len=MAXBUF) :: arg
  integer :: rs
  integer,allocatable :: rseed(:)

  !----------------------------------------------------------------
  ! MPI related variables
  !----------------------------------------------------------------

  integer :: rc,nlocal,myid,nprocs,cmdlineok
  integer :: nlow,nhigh,left,right
  integer,allocatable :: nlocals(:)
  real(rk),allocatable :: xbuff(:),epbuff(:)
  real(rk) :: vsumlocal,vsumglobal,vsumglobal1
  real(rk) :: epsumglobal,eksumglobal
  real(rk) :: t1,t2




  !----------------------------------------------------------------
  ! Executable code begins here
  !----------------------------------------------------------------

  call mpi_init(rc)
  call mpi_comm_rank(mpi_comm_world,myid,rc)
  call mpi_comm_size(mpi_comm_world,nprocs,rc)


  !----------------------------------------------------------------
  ! On proc 0 get number of atoms, time step and simulation length
  ! from command line
  !----------------------------------------------------------------

  if (myid==0) then
     ia=command_argument_count()
     if (ia<4.or.ia>6) then
        call get_command_argument(0,arg)
        write(6,'(/,a,a,a)') 'usage: ',trim(arg),' nat dt maxt vsc [eout [cout]]'
        write(6,'(a)')       '    nat  = number of atoms'
        write(6,'(a)')       '    dt   = time step'
        write(6,'(a)')       '    maxt = number of time steps in simulation'
        write(6,'(a)')       '    vsc = mean velocity of atoms in the beginning (''temperature'')'
        write(6,'(a)')       '    eout = interval for printing energies to stdout'
        write(6,'(a,/)')     '    cout = interval for printing coordinates to ''fort.10'''
        cmdlineok=0
     else
        cout=0
        eout=1
        call get_command_argument(1,arg); read(arg,*) nat
        call get_command_argument(2,arg); read(arg,*) dt
        call get_command_argument(3,arg); read(arg,*) maxt
        call get_command_argument(4,arg); read(arg,*) vsc
        if (ia>4) then
           call get_command_argument(5,arg); read(arg,*) eout
        end if
        if (ia>5) then
           call get_command_argument(6,arg); read(arg,*) cout
        end if
        cmdlineok=1
     end if
  end if


  !----------------------------------------------------------------
  ! Broadcast arguments to all processes
  !----------------------------------------------------------------

  call mpi_bcast(cmdlineok,1,mpi_integer,0,mpi_comm_world,rc)
  if (cmdlineok==0) then
     call mpi_finalize(rc)
     stop
  end if
  call mpi_bcast(nat,1,mpi_integer,0,mpi_comm_world,rc)
  call mpi_bcast(dt,1,mpi_double_precision,0,mpi_comm_world,rc)
  call mpi_bcast(maxt,1,mpi_integer,0,mpi_comm_world,rc)
  call mpi_bcast(vsc,1,mpi_double_precision,0,mpi_comm_world,rc)
  call mpi_bcast(eout,1,mpi_integer,0,mpi_comm_world,rc)
  call mpi_bcast(cout,1,mpi_integer,0,mpi_comm_world,rc)


  !----------------------------------------------------------------
  ! Calculate the local number of atoms and their coordinates
  ! (Load balancing could be improved.)
  !----------------------------------------------------------------

  nlocal=nat/nprocs
  nlow=myid*nlocal+1
  if (myid==nprocs-1) then
     nlocal=nat-(nprocs-1)*nlocal
  end if
  nhigh=nlow+nlocal-1

  allocate(x(0:nlocal+1),v(nlocal),v0(nlocal),a(nlocal),ep(nlocal),ek(nlocal))
  do i=nlow,nhigh
     x(i-nlow+1)=i
  end do

  allocate(nlocals(0:nprocs-1))
  call mpi_gather(nlocal,1,mpi_integer,nlocals,1,mpi_integer,0,mpi_comm_world,rc)
  if (myid==0) then
!     print '(a,1000(x,i0))','% Number of particles: ',nlocals
     allocate(xbuff(1:maxval(nlocals)),epbuff(1:maxval(nlocals)))
  end if

  box=nat


  !----------------------------------------------------------------
  ! Initialize atoms positions and give them random velocities
  !----------------------------------------------------------------

  call random_seed(size=rs)
  allocate(rseed(rs))
  rseed=getseed(1,0)
  call random_seed(put=rseed)

  call random_number(v)
  v=vsc*v


  !----------------------------------------------------------------
  ! Remove center of mass velocity
  !----------------------------------------------------------------

  vsumlocal=sum(v)
  call mpi_allreduce(vsumlocal,vsumglobal,1,mpi_double_precision,mpi_sum,mpi_comm_world,rc)

  v=v-vsumglobal/nat
  vsumlocal=sum(v)
  call mpi_allreduce(vsumlocal,vsumglobal1,1,mpi_double_precision,mpi_sum,mpi_comm_world,rc)


  !----------------------------------------------------------------
  ! Find out neighbor processors
  !----------------------------------------------------------------

  if (myid==0) then
     left=nprocs-1
  else
     left=myid-1
  end if
  if (myid==nprocs-1) then
     right=0
  else
     right=myid+1
  end if


  !----------------------------------------------------------------
  ! If the user desires to, calculate initial energy and 
  ! print initial coords
  !----------------------------------------------------------------

  n=0
  if (cout>0) then
     call exchange_border_atoms(x(1),x(nlocal),x(0),x(nlocal+1),left,right,box)
     do i=1,nlocal
        call accel(i,ep(i),a(i))
     end do
     call printcoords()
  end if


  !----------------------------------------------------------------
  ! Simulation proper: loop over time steps
  !----------------------------------------------------------------

  t1=mpi_wtime()

  time_loop: do n=1,maxt

     v0=v

     call exchange_border_atoms(x(1),x(nlocal),x(0),x(nlocal+1),left,right,box)

     ! Calculate potential energy and acceleration

     atom_loop1: do i=1,nlocal
        call accel(i,ep(i),a(i))
     end do atom_loop1

     if (cout>0) then
        if (mod(n,cout)==0) call printcoords()
     end if

     atom_loop2: do i=1,nlocal

        ! Leap frog integration algorithm: update position and velocity

        v(i)=v(i)+dt*a(i)
        x(i)=x(i)+dt*v(i)

        ! Calculate kinetic energy (note: mass=1)

        ek(i)=1.0/2.0*((v0(i)+v(i))/2.0)**2

     end do atom_loop2


     ! Calculate and print total potential end kinetic energies
     ! and their sum that should be conserved.

     if (eout>0) then
        if (mod(n,eout)==0) then
           epsum=sum(ep)
           eksum=sum(ek)
           call mpi_reduce(epsum,epsumglobal,1,mpi_double_precision,mpi_sum,0,mpi_comm_world,rc)
           call mpi_reduce(eksum,eksumglobal,1,mpi_double_precision,mpi_sum,0,mpi_comm_world,rc)
           if (myid==0) print '(4g20.10)',dt*n,epsumglobal+eksumglobal,epsumglobal,eksumglobal
        end if
     end if

  end do time_loop

  t2=mpi_wtime()

  if (myid==0) print '(a,g20.10)','% Wall clock time ',t2-t1

  call mpi_finalize(rc)  
  stop



contains


  !----------------------------------------------------------------
  ! Send border atoms to neighbor processors
  !----------------------------------------------------------------

  subroutine exchange_border_atoms(x1,xn,x0,xn1,left,right,box)
    implicit none
    real(rk),intent(in)  :: x1,xn,box
    real(rk),intent(out) :: x0,xn1
    integer,intent(in) :: left,right
    integer,parameter :: tag=999
    integer :: status(mpi_status_size)

    ! Send my last atom to the right-hand side neighbor
    ! and receive the zeroth atom from the left-hand side neighbor.

    call mpi_sendrecv(xn,1,mpi_double_precision,right,tag, &
         &            x0,1,mpi_double_precision,left, tag, &
         &            mpi_comm_world,status,rc)

    ! Send my last atom to the left-hand side neighbor 
    ! and receive the (nlocal+1)th atom from the right-hand side neighbor

    call mpi_sendrecv(x1, 1,mpi_double_precision,left, tag, &
         &            xn1,1,mpi_double_precision,right,tag, &
         &            mpi_comm_world,status,rc)

    ! A convenient way to take into account
    ! periodic boundary conditions

    if (myid==0)        x0=x0-box
    if (myid==nprocs-1) xn1=xn1+box

    return
  end subroutine exchange_border_atoms


  !----------------------------------------------------------------
  ! Calculate potential energy and acceleration of atom i
  !----------------------------------------------------------------

  subroutine accel(i,u,a)
    ! Calculate the potential energy u 
    ! and acceleration a of atom i.
    integer,intent(in) :: i
    real(rk),intent(out) :: u,a
    real(rk),parameter :: d=1.0,k1=1.0,k2=0.5
    integer :: j,k
    real(rk) :: dxl,dxr

    j=i-1
    k=i+1

    dxl=x(i)-x(j)
    dxr=x(k)-x(i)
    if (dxl<-box/2.0) dxl=dxl+box
    if (dxl>=box/2.0) dxl=dxl-box
    if (dxr<-box/2.0) dxr=dxr+box
    if (dxr>=box/2.0) dxr=dxr-box
    dxl=dxl-d;
    dxr=dxr-d;

    u=(k1*(dxl**2+dxr**2)+k2*(dxl**3+dxr**3))/2.0
    a=-(2.0*k1*(dxl-dxr)+3.0*k2*(dxl**2-dxr**2))

    return
  end subroutine accel


  !----------------------------------------------------------------
  ! Print coordinates, taken care by one process at a time 
  ! by the root process (myid==0)
  !----------------------------------------------------------------

  subroutine printcoords()
    integer :: ia,ip
    integer,parameter :: tag=999
    real(rk),parameter :: xsc=2.35
    integer :: status(mpi_status_size)

    call mpi_barrier(mpi_comm_world,rc)

    if (myid==0) then

       ! Print root's local atoms
       write(10,*) nat
       write(10,'(a,x,i0,x,a,3f14.4)') 'timestep ',n,'boxsize',box,0.0,0.0
       do ia=1,nlocal
          write(10,'(a,x,4g20.10)') 'Si',xsc*x(ia),0.0,0.0,ep(ia)
       end do

       ! Receive and print other processes' atoms
       do ip=1,nprocs-1
          call mpi_recv(xbuff,nlocals(ip),mpi_double_precision,ip,tag,mpi_comm_world,status,rc)
          call mpi_recv(epbuff,nlocals(ip),mpi_double_precision,ip,tag,mpi_comm_world,status,rc)
          do ia=1,nlocals(ip)
             write(10,'(a,x,4g20.10)') 'Si',xsc*xbuff(ia),0.0,0.0,epbuff(ia)
          end do
       end do

    else

       ! Send atoms to root
       call mpi_send(x(1:nlocal),nlocal,mpi_double_precision,0,tag,mpi_comm_world,rc)
       call mpi_send(ep(1:nlocal),nlocal,mpi_double_precision,0,tag,mpi_comm_world,rc)

    end if

    return
  end subroutine printcoords

end program md1d


!----------------------------------------------------------------
! This is just RNG seed generator.
!----------------------------------------------------------------


!----------------------------------------------------!
!                                                    !
!  Get the RNG seed from /dev/urandom device.        !
!  In order to get positive seed the most            !
!  significant bit in the number read from the       !
!  device is cleared (by anding it with LMASK).      !
!                                                    !
!  If the device can not be opened or read           !
!  routine falls back to calculating seed from       !
!  the current time.                                 !
!                                                    !
!  Note that the stream access of a file is used     !
!  which is a Fortran 2003 feature.                  !
!                                                    !
!  Input parameters:                                 !
!    info : integer, if /=0 print info               !
!    file : integer, 0: use /dev/urandom             !
!                    1: use /dev/random              !
!                                                    !
!  Note that if you use /dev/random to generate many !
!  values it may take a long time because the        !
!  quality of random bits from /dev/random is        !
!  guaranteed.                                       !
!                                                    !
!  A.Kuronen, antti.kuronen@helsinki.fi, 2008,2009   !
!                                                    !
!----------------------------------------------------!

integer function getseed(info,file)
  implicit none
  integer,intent(in) :: info,file
  integer :: t(8),rn,is
  integer,parameter :: LMASK=2147483647
  integer,parameter :: LUN=676767
  character (len=80) :: rdev0='/dev/urandom',rdev1='/dev/random',rdev
  logical :: openok,readok

  openok=.true.
  readok=.true.

  if (file==0) then
     rdev=rdev0
  else
     rdev=rdev1
  end if
  open(LUN,file=rdev,form='unformatted',access='stream',action='read',iostat=is)
  if (is/=0) then
     openok=.false.
     print *,'open',is
  else
     read(LUN,iostat=is) rn
     if (is/=0) then
        readok=.false.
     end if
  end if
  if (openok) close(LUN)

  if (openok.and.readok) then
     rn=iand(rn,LMASK)
!     if (info/=0) write(0,'(a,a,a,i12)') '% Seed from ',trim(rdev),':',rn
  else
     call date_and_time(values=t)
     rn=t(7)+60*(t(6)+60*(t(5)+24*(t(3)-1+31*(t(2)-1+12*t(1)))))+t(8)
!     if (info/=0) write(0,'(a,i12)') '% Seed from time:',rn
  end if

  getseed=rn

  return
end function getseed

