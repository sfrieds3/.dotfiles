k8s_pod_stats() {
    local pod_name=""
    local namespace=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -n|--namespace)
                namespace="$2"
                shift 2
                ;;
            -h|--help)
                echo "Usage: k8s_pod_stats [-n namespace] <pod_name>"
                echo "       k8s_pod_stats <pod_name> [namespace]"
                echo ""
                echo "Display memory and CPU stats for a Kubernetes pod."
                return 0
                ;;
            *)
                if [[ -z "$pod_name" ]]; then
                    pod_name="$1"
                elif [[ -z "$namespace" ]]; then
                    namespace="$1"
                fi
                shift
                ;;
        esac
    done

    if [[ -z "$pod_name" ]]; then
        echo "Error: Pod name required" >&2
        echo "Usage: k8s_pod_stats [-n namespace] <pod_name>" >&2
        return 1
    fi

    local -a kubectl_args=()
    [[ -n "$namespace" ]] && kubectl_args+=(-n "$namespace")

    echo "Stats for pod: $pod_name${namespace:+ (namespace: $namespace)}"
    echo "========================================"

    local mem_bytes
    mem_bytes=$(kubectl exec "${kubectl_args[@]}" "$pod_name" -- cat /sys/fs/cgroup/memory/memory.usage_in_bytes 2>/dev/null)
    if [[ $? -eq 0 ]]; then
        echo "Memory: $((mem_bytes / 1024 / 1024)) MB"
    else
        mem_bytes=$(kubectl exec "${kubectl_args[@]}" "$pod_name" -- cat /sys/fs/cgroup/memory.current 2>/dev/null)
        if [[ $? -eq 0 ]]; then
            echo "Memory: $((mem_bytes / 1024 / 1024)) MB"
        else
            echo "Memory: Could not retrieve"
        fi
    fi

    echo -n "CPU: Calculating..."
    local cpu1 cpu2 cpu_diff millicores start_time elapsed

    start_time=$EPOCHREALTIME
    cpu1=$(kubectl exec "${kubectl_args[@]}" "$pod_name" -- cat /sys/fs/cgroup/cpu/cpuacct.usage 2>/dev/null)
    if [[ $? -eq 0 ]]; then
        sleep 1
        cpu2=$(kubectl exec "${kubectl_args[@]}" "$pod_name" -- cat /sys/fs/cgroup/cpu/cpuacct.usage 2>/dev/null)
        elapsed=$(( EPOCHREALTIME - start_time ))
        cpu_diff=$((cpu2 - cpu1))
        millicores=$(( cpu_diff / 1000000 / elapsed ))
        printf "\rCPU: %dm (millicores)              \n" "$millicores"
    else
        start_time=$EPOCHREALTIME
        cpu1=$(kubectl exec "${kubectl_args[@]}" "$pod_name" -- cat /sys/fs/cgroup/cpu.stat 2>/dev/null | awk '/usage_usec/ {print $2}')
        if [[ -n "$cpu1" ]]; then
            sleep 1
            cpu2=$(kubectl exec "${kubectl_args[@]}" "$pod_name" -- cat /sys/fs/cgroup/cpu.stat 2>/dev/null | awk '/usage_usec/ {print $2}')
            elapsed=$(( EPOCHREALTIME - start_time ))
            cpu_diff=$((cpu2 - cpu1))
            millicores=$(( cpu_diff / 1000 / elapsed ))
            printf "\rCPU: %dm (millicores)              \n" "$millicores"
        else
            printf "\rCPU: Could not retrieve            \n"
        fi
    fi
}

_k8s_pod_stats() {
    local context state state_descr line
    typeset -A opt_args

    _arguments -C \
        '-n[Namespace]:namespace:->namespaces' \
        '--namespace[Namespace]:namespace:->namespaces' \
        '-h[Show help]' \
        '--help[Show help]' \
        '1:pod:->pods' \
        '2:namespace:->namespaces'

    case "$state" in
        namespaces)
            local -a namespaces
            namespaces=(${(f)"$(kubectl get namespaces -o jsonpath='{range .items[*]}{.metadata.name}{"\n"}{end}' 2>/dev/null)"})
            _describe 'namespace' namespaces
            ;;
        pods)
            local ns="${opt_args[-n]:-${opt_args[--namespace]:-}}"
            local -a pods
            if [[ -n "$ns" ]]; then
                pods=(${(f)"$(kubectl get pods -n "$ns" -o jsonpath='{range .items[*]}{.metadata.name}{"\n"}{end}' 2>/dev/null)"})
            else
                pods=(${(f)"$(kubectl get pods -o jsonpath='{range .items[*]}{.metadata.name}{"\n"}{end}' 2>/dev/null)"})
            fi
            _describe 'pod' pods
            ;;
    esac
}

compdef _k8s_pod_stats k8s_pod_stats
