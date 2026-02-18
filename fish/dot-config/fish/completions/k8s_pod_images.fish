complete -c k8s_pod_images -s n -l namespace -xa '(kubectl get namespaces -o jsonpath="{range .items[*]}{.metadata.name}\n{end}" 2>/dev/null)'
complete -c k8s_pod_images -f -a '(kubectl get pods -o jsonpath="{range .items[*]}{.metadata.name}\n{end}" 2>/dev/null)'
