function drun -d "Select and run a Docker image with fzf"
    set -l args $argv
    set -l selection (docker images --format '{{.Repository}}:{{.Tag}}\t{{.ID}}' | sort -u | fzf --prompt="Select image: " --header='NAME:TAG\tIMAGE ID')
    or return 1

    if test -z "$selection"
        return 1
    end

    set -l image (string split \t $selection)[1]
    echo "Running: docker run $args $image"
    docker run $args $image
end
