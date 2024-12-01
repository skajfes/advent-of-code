#!/bin/bash

die() {
	echo >&2 "$@"
	exit 1
}

[ "$#" -eq 2 ] || die "2 arguments required, $# provided"
echo $1 | grep -E -q '^[0-9]+$' || die "Number required, $1 provided"
echo $2 | grep -E -q '^[0-9]+$' || die "Number required, $1 provided"

Year=$1
Day=$2

pushd "$(dirname "$0")" > /dev/null

if [ ! -d "$Year" ]; then
	echo "Creating solution AdventOfCode_$Year"
	dotnet new sln -n "AdventOfCode_$Year" -o $Year
fi

pushd $Year > /dev/null
p=$(printf "Day%02d" $Day)
echo "Creating project $p"
dotnet new console -lang "F#" -o $p
dotnet sln add $p/$p.fsproj

popd > /dev/null

cp Program.fs.template $Year/$p/Program.fs

popd > /dev/null

./add-input.sh $Year $Day
