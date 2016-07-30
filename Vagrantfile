
$machine = <<MACHINE

apt-get update
apt-get install haskell-platform -y

MACHINE

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "hashicorp/precise64"
  config.vm.provision :shell, inline: $machine
  config.vm.box_check_update = false
end
