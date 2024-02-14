#!/bin/bash

if [ "$(whoami)" != "root" ]; then
	echo "This script must be executed as root. Please use SUDO command."
	exit 1
fi

# copy programs to computer
echo -e "Copying software..."
mkdir ~/feasa

if [ -f "./FeasaTerminal" ];
then
	cp ./FeasaTerminal ~/feasa/
	sudo chmod a+x ~/feasa/FeasaTerminal
	cp ./help_feasa_terminal.epub ~/feasa/
fi
if [ -f "./Spectrometer" ];
then
	cp ./Spectrometer ~/feasa/
	sudo chmod a+x ~/feasa/Spectrometer
	cp ./help_spectrometer.epub ~/feasa/
fi
if [ -f "./TestSoftware" ];
then
	cp ./TestSoftware ~/feasa/
	sudo chmod a+x ~/feasa/TestSoftware
	cp ./help_test_software.epub ~/feasa/
fi
if [ -f "./UserCal" ];
then
	cp ./UserCal ~/feasa/
	sudo chmod a+x ~/feasa/UserCal
	cp ./help_usercal.epub ~/feasa/
fi
if [ -f "./UserSoft" ];
then
	cp ./UserSoft ~/feasa/
	sudo chmod a+x ~/feasa/UserSoft
	cp ./help_user_software.epub ~/feasa/
fi
if [ -f "./DisplayTester" ];
then
	cp ./DisplayTester ~/feasa/
	sudo chmod a+x ~/feasa/DisplayTester
fi

# include repositories (http://www.mono-project.com/docs/getting-started/install/linux/)
echo -e "Adding repositories..."
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list

echo -e "Updating sources..."
sudo apt-get update

# install libraries
echo -e "Installing required packages..."
sudo apt-get install mono-runtime -y
sudo apt-get install libgdiplus -y
sudo apt-get install libmono-system-core4.0-cil -y
sudo apt-get install libmono-system-windows-forms4.0-cil -y

# add user to group dialout
useradd $(printf '%s' ${SUDO_USER:-$USER}) dialout &> /dev/null
rc=$?;
if [[ $rc != 0 ]]; then
	gpasswd --add $(printf '%s' ${SUDO_USER:-$USER}) dialout &> /dev/null
	rc=$?;
	if [[ $rc != 0 ]]; then
		echo "Unable to add user ${SUDO_USER:-$USER} to group dialout"
	else
		echo "User ${SUDO_USER:-$USER} added to group dialout"
	fi
else
	echo "User ${SUDO_USER:-$USER} added to group dialout"
fi

echo -e "--------------------"
echo -e "Done!"
echo -e "Software has been copied to ~/feasa"
echo -e "Please, restart computer for the changes to take effect"
echo -e ""


