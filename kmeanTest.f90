!#define DEBUG 
program main
use kmeans 
implicit none

character(len=32)	:: arg
integer				:: numObjects, numAttributes, numClusters

type(data_struct)	:: data_in[*]
type(data_struct)	:: clusters
type(data_struct)	:: data_overall !only valid in master image(this_image() == 1)

integer				:: numSlaves, parNumObjects, remain, maxNumObjects
integer,allocatable	:: procNumObjects(:), offsets(:)
integer				:: i	
integer, parameter	:: max_iterations=50
#ifdef DEBUG
character(len=64)   :: fmtstr
#endif


!!Get arguments
if(COMMAND_ARGUMENT_COUNT() < 3) then
	call error_message()
	error stop "Incomplete arguments"
end if

!Usage: kmeanTest numObjects numAttributes numClusters
call GET_COMMAND_ARGUMENT(1, arg)
read(arg, '(I10)') numObjects

call GET_COMMAND_ARGUMENT(2, arg)
read(arg, '(I10)') numAttributes

call GET_COMMAND_ARGUMENT(3, arg)
read(arg, '(I10)') numClusters
#ifdef DEBUG
write(6, '(A, I4, A, I4, A, I4)'), 'numObject=', numObjects, ' numAttributes=', numAttributes, ' numClusters=', numClusters
#endif

sync all

!!Initialize processes
!!The image 1 will store all informations, 
!!other images, as slave images, involve in processing?
numSlaves = num_images() - 1
parNumObjects =  numObjects/numSlaves

remain = numObjects - parNumObjects*numSlaves

allocate(procNumObjects(num_images()))
allocate(offsets(num_images()))
procNumObjects(1) = 0

do i = 2, num_images()
	if (i-1<remain) then
		procNumObjects(i) = parNumObjects+1
	else
		procNumObjects(i) = parNumObjects
	end if
end do 

offsets(1) = 1

do i = 2, num_images()
	offsets(i) = offsets(i-1) + procNumObjects(i-1)
end do

maxNumObjects = maxval(procNumObjects(:)) !the max number of parNumObjects

procNumObjects(1) = numObjects

#ifdef DEBUG
write(fmtstr, '(A,I,A)') '(A,',size(procNumObjects),'I4)'
write(6, fmt=fmtstr) 'procNumObjects=',procNumObjects
write(6, fmt=fmtstr) 'offsets=',offsets
#endif

!!Memory Allocation
!if(this_image() == 1) then
!	data_in%leading_dim = numAttributes
!	data_in%secondary_dim = numObjects
!	allocate(data_in%dataset(numAttributes, numObjects))
!	allocate(data_in%members(numObjects))
!else
!	numObjects = procNumObjects(this_image())
!	data_in%leading_dim = numAttributes
!	data_in%secondary_dim = numObjects
!	allocate(data_in%dataset(numAttributes, numObjects))
!	allocate(data_in%members(numObjects))
!end if

!Local memory allocation in image 1
if(this_image() == 1) then
	data_overall%leading_dim = numAttributes
	data_overall%secondary_dim = numObjects
	allocate(data_overall%dataset(numAttributes, numObjects))
	allocate(data_overall%members(numObjects))
end if

!!Data_in coarray allocation on all images
!numObjects = procNumObjects(this_image())
data_in%leading_dim = numAttributes
data_in%secondary_dim = procNumObjects(this_image())
allocate(data_in%dataset(numAttributes, maxNumObjects))
allocate(data_in%members(maxNumObjects))

clusters%leading_dim = numAttributes
clusters%secondary_dim = numClusters
allocate(clusters%dataset(numAttributes, numClusters))
allocate(clusters%members(numClusters))

!!Intialize dataset in image 1
if(this_image() == 1) then
!	call random_initialization(data_overall)
!	call initialize_cluster(data_in, clusters)
	call readInDataset(data_overall, "data.dat")
	write(*, '(A)') "Dataset initialized"
end if

!!Distribute dataset
!do i = 2, num_images()
!	data_in[i]%dataset(:,1:procNumObjects(i)) = data_overall%dataset(:,offsets(i):offsets(i)+procNumObjects(i)-1)
!
!end do

!!main part, Cluster data
call cluster(data_in, clusters, max_iterations)

!!Save final cluster indexes
!! why should I recollecting these dataset?
if(this_image() .ne. 1 ) then
	!write back their members
end if

!! Save data to files

!! clean memory
!call clean(data_in)
!call clean(clusters)

contains
subroutine error_message()
	write(6, '(A)') 'Error using kmeans: Three arguments required: &
	& kmeans numElements numAttributes numClusters'
end subroutine error_message
!Intialization subroutines

subroutine random_initialization(data_in)
	implicit none
	type(data_struct), intent(inout)	::data_in
	!Intialize the dataset with random number
	integer				:: m,n,i,j

	m = data_in%secondary_dim
	n = data_in%leading_dim

	do i=1, m
		data_in%members(i) = 0
		do j=1,n
!			data_in%dataset(i,j) = rand()/RAND_MAX
		end do
	end do
end subroutine random_initialization
subroutine initialize_clusters(data_in, cluster_in)
	!Randomly pick initial cluster centers
	implicit none
	type(data_struct), intent(in)	:: data_in[*]
	type(data_struct), intent(inout):: cluster_in

	integer							:: i,j,pick,&
										n,m,Objects,step

	n = cluster_in%leading_dim !numAttributes
	m = cluster_in%secondary_dim!numClusters
	Objects = data_in%secondary_dim!numObjects

	step = Objects/m

	do i = 1, m
		do j = 1, n
		cluster_in%dataset(i,j) = data_in%dataset(pick,j) 
		end do
		pick = pick + step
	end do
end subroutine initialize_clusters


!!This routine is used for read in stored dataset
!!Dataset start with numTotalData, numAttributes
!!following with numTotalData points pair
subroutine readInDataset(data_in, fileName)
	implicit none
	type(data_struct), intent(inout)	:: data_in
	character(len=*)					:: filename
    character(len=32)                   :: linefmt
	integer		                        :: numAttributes, numObjects,i,j
	open(2, file=filename)
	read(2, '(I,I)') numObjects, numAttributes
#ifdef DEBUG
	write(6, '(A, I4, A, I4)'), 'In datafile, numObject=', numObjects, ' numAttributes=', numAttributes	
#endif
   do j=1,numObjects 
    read(2, *) data_in%dataset(:,j)
   end do
   data_in%leading_dim=numAttributes
   data_in%secondary_dim=numObjects
#ifdef DEBUG
   call print_dataset(data_in)
#endif
	close(2)
end subroutine readInDataset
end program main
