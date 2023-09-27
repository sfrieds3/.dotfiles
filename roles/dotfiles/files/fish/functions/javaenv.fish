function javaenv
    if test (count $argv) -eq 0
        /usr/libexec/java_home -V
    else
        switch $argv[1]
            case ls
                /usr/libexec/java_home -V
            case set
                set _java_home (/usr/libexec/java_home -v $argv[2])
                set --global --export JAVA_HOME $_java_home
                echo "JAVA_HOME=$_java_home"
        end
    end
end
