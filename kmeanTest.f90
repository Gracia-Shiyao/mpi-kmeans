program main
use kmeans 
implicit none

character(len=32)	:: arg
integer				:: numObjects, numAttributes, numClusters

type(data_struct)	:: data_in[*]
type(data_struct)	:: clusters
type(data_struct)	:: data_overall !only valid in master image(this_image() == 1)

integer				:: numSlaves, parNumObjects, remain, maxNumObjects
integer				:: procNumObjects(:), offsets(:)
integer				:: i
!!Get arguments
if(iargs() < 3) 
	call error_message()
	error stop "Incomplete arguments"
end if

!Usage: kmeanTest numObjects numAttributes numClusters
call getarg(1, arg)
read(arg, '(I10)') numObjects

call getarg(2, arg)
read(arg, '(I10)') numAttributes

call getarg(3, arg)
read(arg, '(I10)') numClusters

!!Initialize processes
!!The image 1 will store all informations, 
!!other images, as slave images, involve in processing?
numSlaves = num_images() - 1
parNumObjects =  numObjects/numSlaves

remain = numObjects - parNumObjects*numSlaves

allocate(procNumObjects(num_images()))

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
allocate(data_in%dataset(numAtrribute, maxNumObjects))
allocate(data_in%members(maxNumObjects))

clusters%leading_dim = numAttributes
clusters%secondary_dim = numClusters
allocate(clusters%dataset(numAttributes, numClusters))
allocate(clusters%members(numClusters))

!!Intialize dataset in image 1
if(this_image() == 1) then
	call random_initialization(data_overall)
!	call initialize_cluster(data_in, clusters)
	write(*, '(A)') "Dataset initialized"
end if

!!Distribute dataset
do i = 2, num_images()
	data_in[i]%dataset(:,1:procNumObjects(i)) = data_overall%dataset(:,)

end do

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
	write(*, ('A,A')) "Error using kmeans: Three arguments required:\n", &
	"kmeans numElements numAttributes numClusters"
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
			data_in%dataset(i,j) = rand()/RAND_MAX
		end do
	end do
end subroutine random_initialization
subroutine intialize_clusters(data_in, cluster_in)
	!Randomly pick initial cluster centers
	implicit none
	type(data_struct), intent(in)	:: data_in
	type(data_struct), intent(inout):: cluster_in

	integer							:: i,j,pick,&
										n,m,Objects,step

	n = cluster_in%leading_dim !numAttributes
	m = cluster_in%secondary_dim!numClusters
	Objects = data_in%secondary_dim!numObjects

	step = Objects/m

	do i = 1, m
	end do
end subroutine initialize_clusters
end program main