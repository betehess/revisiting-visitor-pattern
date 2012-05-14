#!/bin/sh

dir=$(dirname $0)
cd "$dir"

url="http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.11.3/sbt-launch.jar"

sbt="sbt-launch-0.11.3.jar"

# set the right tool to download sbt
if [ -n "$tool" ]; then
    echo -n
elif [ -n "$(which wget)" ]; then
    tool="wget"
elif [ -n "$(which curl)" ]; then
    tool="curl"
else
    echo "Couldn't find a tool to download sbt. Please do the following"
    echo "* download $url"
    echo "* set the name of the file to $sbt"
    echo "* relaunch ./sbt"
    exit 1
fi

# download the sbt launcher if it's not already here
if [ ! -f "$sbt" ]; then
    case "$tool" in
        "wget"*)
            wget "$url" -O "./$sbt"
            ;;
        "curl"*)
            curl "$url" -o "./$sbt"
            ;;
        *)
            echo "don't know this tool: $tool"
            exit 2
    esac
fi

# tweak this line according to your needs
java $SBT_PROPS -Xmx512M -jar -Dfile.encoding=UTF8 -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m "$dir/$sbt" "$@"

