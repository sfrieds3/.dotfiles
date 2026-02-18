function k8s_pod_stats -d "Display memory and CPU stats for a Kubernetes pod"
    argparse h/help 'n/namespace=' -- $argv
    or return 1

    if set -q _flag_help
        echo "Usage: k8s_pod_stats [-n namespace] <pod_name>"
        echo "       k8s_pod_stats <pod_name> [namespace]"
        echo ""
        echo "Display memory and CPU stats for a Kubernetes pod."
        return 0
    end

    set -l pod_name
    set -l namespace $_flag_namespace

    for arg in $argv
        if not set -q pod_name[1]
            set pod_name $arg
        else if test -z "$namespace"
            set namespace $arg
        end
    end

    if not set -q pod_name[1]
        echo "Error: Pod name required" >&2
        return 1
    end

    set -l kubectl_args
    if test -n "$namespace"
        set kubectl_args -n $namespace
    end

    if test -n "$namespace"
        echo "Stats for pod: $pod_name (namespace: $namespace)"
    else
        echo "Stats for pod: $pod_name"
    end
    echo "========================================"

    set -l mem_bytes (kubectl exec $kubectl_args $pod_name -- cat /sys/fs/cgroup/memory/memory.usage_in_bytes 2>/dev/null)
    if test $status -eq 0
        echo "Memory: "(math "$mem_bytes / 1024 / 1024")" MB"
    else
        set mem_bytes (kubectl exec $kubectl_args $pod_name -- cat /sys/fs/cgroup/memory.current 2>/dev/null)
        if test $status -eq 0
            echo "Memory: "(math "$mem_bytes / 1024 / 1024")" MB"
        else
            echo "Memory: Could not retrieve"
        end
    end

    echo -n "CPU: Calculating..."

    set -l start_time (command date +%s)
    set -l cpu1 (kubectl exec $kubectl_args $pod_name -- cat /sys/fs/cgroup/cpu/cpuacct.usage 2>/dev/null)
    if test $status -eq 0
        sleep 1
        set -l cpu2 (kubectl exec $kubectl_args $pod_name -- cat /sys/fs/cgroup/cpu/cpuacct.usage 2>/dev/null)
        set -l elapsed (math (command date +%s)" - $start_time")
        if test $elapsed -eq 0
            set elapsed 1
        end
        set -l cpu_diff (math "$cpu2 - $cpu1")
        set -l millicores (math "$cpu_diff / 1000000 / $elapsed")
        printf "\rCPU: %dm (millicores)              \n" $millicores
    else
        set start_time (command date +%s)
        set cpu1 (kubectl exec $kubectl_args $pod_name -- cat /sys/fs/cgroup/cpu.stat 2>/dev/null | awk '/usage_usec/ {print $2}')
        if test -n "$cpu1"
            sleep 1
            set -l cpu2 (kubectl exec $kubectl_args $pod_name -- cat /sys/fs/cgroup/cpu.stat 2>/dev/null | awk '/usage_usec/ {print $2}')
            set -l elapsed (math (command date +%s)" - $start_time")
            if test $elapsed -eq 0
                set elapsed 1
            end
            set -l cpu_diff (math "$cpu2 - $cpu1")
            set -l millicores (math "$cpu_diff / 1000 / $elapsed")
            printf "\rCPU: %dm (millicores)              \n" $millicores
        else
            printf "\rCPU: Could not retrieve            \n"
        end
    end
end
