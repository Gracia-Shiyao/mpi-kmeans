module kmeans

implicit none

type data_struct 
	real(kind=8),allocatable		:: dataset(:,:) !dataset(numAttributes, numObjects)
	integer(kind=8),allocatable		:: members(:) !member(numObjects)
	integer				:: leading_dim
	integer				:: secondary_dim
end type 
real(kind=8), parameter				:: threshold=0.01
contains
	subroutine print_dataset(data_in) 
	!Print out the dataset in data_in or clusters
		type(data_struct), intent(in)	:: data_in[*]
		integer							:: i

		do i = 1, data_in%leading_dim
			write(*, '(F6.6, F6.6)') data_in%dataset(:, i)
		end do
	end subroutine print_dataset
	
	function euclidean_distance(v1, v2, length) result(dist)
		implicit none
		real(kind=8), intent(in)		:: v1(:)
		real(kind=8), intent(in)		:: v2(:)
		integer		, intent(in)		:: length
		real(kind=8)					:: dist

		integer							:: i

		do i=1, length
			dist = dist + (v1(i) - v2(i))*(v1(i) - v2(i));
		end do
	end function

	!Only slave processes will enter this subroutine
	subroutine kmeans_process(data_in, clusters, newCentroids, SumOfDist, sse)
		implicit none
		type(data_struct), intent(inout)	:: data_in[*] !data_in[2..NumImages]
		type(data_struct), intent(inout):: clusters
		real(kind=8), intent(inout)		:: newCentroids(:,:)!Centroids(numAttributes, numClusters)
		real(kind=8), intent(inout)		:: SumOfDist
		real(kind=8), intent(inout)		:: sse

		integer							:: i,k,j
		real(kind=8)					:: tmp_dist, min_dist
		integer							:: tmp_index

		!zero-fy clusters' members
		clusters%members(:) = 0

		do i = 1, data_in%secondary_dim !foreach data in data_in%dataset
			tmp_dist = 0
			tmp_index = 0
			min_dist = huge(min_dist)

			!Find nearest center
			do k = 1, clusters%secondary_dim
				tmp_dist = euclidean_distance(data_in%dataset(:,i), clusters%dataset(:,k), data_in%leading_dim)
				if(tmp_dist < min_dist) then
					min_dist = tmp_dist
					tmp_index = k
				end if
			end do

			data_in%members(i) =  tmp_index
			SumOfDist = SumOfDist + min_dist
			sse = sse + min_dist*min_dist
			clusters%members(tmp_index) = clusters%members(tmp_index)+1

			do j = 1, data_in%leading_dim !numAttributes
				newCentroids(tmp_index, j) = newCentroids(tmp_index, j) + data_in%dataset(i,j)
			end do
		end do
		!Update cluster centers
		clusters%dataset(:,:) = newCentroids(:,:)

	end subroutine kmeans_process

	subroutine cluster(data_in, clusters, max_iteration)
		implicit none
		type(data_struct), intent(inout)	:: data_in[*]
		type(data_struct), intent(inout)	:: clusters
		integer			 , intent(in)		:: max_iteration

		integer								:: i, j, k, iter, dest
		real(kind=8)						:: SumOfDist, new_SumOfDist, &
												part_SumOfDist, sse, psse
		integer, allocatable				:: part_size(:)[:] !part_size(numClusters)										
		real(kind=8), allocatable			:: newCentroids(:,:)!Centroids(numAttributes, numClusters)
		real(kind=8), allocatable			:: partCentroids(:,:)

		integer								:: endcond 
		!Intialize
		SumOfDist = 0
		new_SumOfDist = 0
		sse = 0
		endcond =0

		!Allocation
		allocate(part_size(clusters%secondary_dim)[*])
		allocate(newCentroids(clusters%leading_dim, clusters%secondary_dim))
		allocate(partCentroids(clusters%leading_dim, clusters%secondary_dim))

		!Iteration
		do iter = 1, max_iteration
			!All process enter this iteration then they split into different execution path
			if(this_image() == 1) then !the master image
				new_SumOfDist = 0
				!set partial cluster's size to zero
				!this array is used to cluster sizes that
				!are received from other processes
				part_size = 0
				clusters%members(:) = 0

				!Broadcast the new clusters' centroid to all images 
				!all images will make this call 
				call co_broadcast(clusters%dataset(:,:), 1)

				newCentroids(:,:) = 0

				!Reduce to take the sum of the partial computed sum of distance

				call co_sum(part_SumOfDist, 1)
				new_SumOfDist = part_SumOfDist

				!Compute the new centroids by summing up the partial new centroids values 
				!reported by the other processes
				call co_sum(partCentroids(:,:), 1)
				newCentroids(:,:) = partCentroids(:,:)

				sync all
				!Sum up partial clusters' size
				call co_sum(part_size(:), 1) 
				clusters%members(:) =  part_size(:)
					

				!Sum up partial clusters' size that is received from other processes
				!do dest=1, NumTask-1
				!	call MPI_Recv(part_size, clusters->secondary_dim, MPI_UNSIGNED, dest, 5, MPI_COMM_WORLD, &stat)
				!	do k=1, clusters%secondary_dim
				!		clusters%members(k) = part_size(k) + clusters%members(k)
				!	end do
				!end do

				!Compute the new center for each cluster
				do k=1, clusters%secondary_dim !numClusters
					clusters%dataset(:,k) = newCentroids(:,k)/clusters%members(k)
				end do

				!Check whether SumOfDist has stabilized
				if(abs(SumOfDist - new_SumOfDist) < threshold) then
					!inform other process to exit the loop
					endcond = 1
					call co_broadcast(endcond, 1)
					exit ! exit the loop
				else 
					call co_broadcast(endcond, 1) !broadcast 0
				end if

				SumOfDist = new_SumOfDist
				write(6, '(A, I6, A, f6.6)') "Sum of Distances of iteration ", iter,": ", new_SumOfDist
			else ! rank > 0
				part_SumOfDist = 0
				psse = 0

				!Receive new clusters' centers from master processes
				call co_broadcast(clusters%dataset(:,:), 1)
				!Set new partCentroids to zero
				partCentroids = 0

				call kmeans_process(data_in, clusters, partCentroids, part_SumOfDist, psse)

				!Sum up total dist on master
				call co_sum(part_SumOfDist, 1)

				!Each slave process sends the partial centroids computed
				call co_sum(partCentroids(:,:), 1)

				!Each slave process sends the partial clusters' sizes
				sync all
				call co_sum(part_size(:), 1)

				!Each slave process receive the condition whether sum of distance has stablized
				call co_broadcast(endcond, 1)
				if(endcond == 1) exit
			end if	
		end do!End of iterations

		if(this_image() == 0) then
			!Reduce partial sse computed
			call co_sum(psse, 1)
			sse = psse
			write(6, '(A, I6, A)') "Finished after ", iter, " iterations"
			write(6, '(A, F6.6)') "SSE equals to ", sse
		else
			call co_sum(psse, 1)
		end if
		!Free memory
		deallocate(newCentroids)
		deallocate(partCentroids)
		deallocate(part_size)
	end subroutine cluster
end module kmeans
