complete -c k8s_pod_stats -s n -l namespace -xa '(kubectl get namespaces -o jsonpath="{range .items[*]}{.metadata.name}\n{end}" 2>/dev/null)'
complete -c k8s_pod_stats -s h -l help -d "Show help"
complete -c k8s_pod_stats -f -a '(kubectl get pods -o jsonpath="{range .items[*]}{.metadata.name}\n{end}" 2>/dev/null)'
