function javaenv
    if test (count $argv) -eq 0
        /usr/libexec/java_home -V
    else
        switch $argv[1]
            case ls
                /usr/libexec/java_home -V
            case set
                set -xU JAVA_HOME (/usr/libexec/java_home -v $argv[2])
        end
    end
end
