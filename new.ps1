
param(
	[Parameter(Mandatory=$true)]
	[int]$Year,
	[Parameter(Mandatory=$true)]
	[int]$Day
)

pushd $PSScriptRoot

if (-not (Test-Path $Year)) {
	dotnet new sln -n "AdventOfCode_$Year" -o $Year
}

pushd $Year
$p = "Day$($Day.ToString("00"))"
dotnet new console -lang "F#" -o $p
dotnet sln add $p/$p.fsproj

popd

cp Program.fs.template $Year/$p/Program.fs

popd

./add-input.ps1 $Year $Day
