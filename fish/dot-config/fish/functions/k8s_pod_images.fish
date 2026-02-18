function k8s_pod_images -d "Show container images for Kubernetes pods"
    argparse 'n/namespace=' -- $argv
    or begin
        echo "Usage: k8s_pod_images [-n|--namespace <namespace>] [pod_name]"
        return 1
    end

    set -l kubectl_args
    if set -q _flag_namespace
        set kubectl_args -n $_flag_namespace
    end
    if set -q argv[1]
        set -a kubectl_args $argv[1]
    end

    kubectl get pods $kubectl_args -o json | jq -r '
    (if .items then .items else [.] end) |
    map({
        namespace: .metadata.namespace,
        name: .metadata.name,
        images: (.spec.containers | map(.image) | join(", "))
    }) |
        (["NAMESPACE", "NAME", "IMAGES"] | @tsv),
    (.[] | [.namespace, .name, .images] | @tsv)
    ' | column -t
end
