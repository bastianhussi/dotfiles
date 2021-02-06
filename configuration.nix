{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    initrd.kernelModules = [ "amdgpu" ];
    loader = {
      efi = {
        efiSysMountPoint = "/boot";
        canTouchEfiVariables = true;
      };
      grub = {
        enable = true;
        version = 2;
        efiSupport = true;
        device = "nodev";
      };
    };
    cleanTmpDir = true;
  };

  networking = {
    hostName = "bastian-desktop";
    networkmanager = {
      enable = true;
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [];
      allowedUDPPorts = [];
      allowPing = false;
    };
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    pulseaudio = {
      enable = true;
      package = pkgs.puluseaudioFull;
      support32Bit = true;
    };

    bluetooth = {
      enable = true;
    };

    opengl = {
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        amdvlk
        rocm-opencl-icd
        rocm-opencl-runtime
      ];
      # NOTE: requires unstable (or 21.03)
      extraPackages32 = with pkgs; [
        driversi686Linux.amdvlk
      ];
    };
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "de_DE.UTF-8";
  };

  time = {
    timeZone = "Europe/Berlin";
  };

  fonts = {
    enableFontsDir = true;
    antialias = true;
    hinting = {
      enable = true;
      autohint = false;
    };
    fontsconfig = {
      defaultFonts = {
        emoji = "Noto Color Emoji";
        sansSerif = ["Noto Sans" "DejaVu Sans" "Liberation Sans"]
      };
    };
    subpixel = {
      lcdfilter = "default";
      rgba = "rgb";
    };
    fonts = with pkgs; [
      corefonts roboto
      liberation_ttf dejavu_fonts
      terminus_fonts source-code-pro
      fira-code fira-code-symbols
      jetbrains-mono
      nerdfonts
    ];
  };

  programs = {
    fish = {
      enable = true;
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    tree wget git gnupg curl firefox unzip
    go openjdk nodejs watchman rustup
    rust-analyzer gopls go-tools
    exercism
    android-studio jetbrains.idea-community jetbrains.pycharm-community
    vscodium
    vivaldi vivaldi-widevine vivaldi-ffmpeg-codecs
    tor-browser-bundle-bin
    teams
    krita drawio gimp inkscape
    texlive.combined.scheme-medium
    insomnia dbeaver
    kubectl minikube
    steam
    wine-staging # vs. winePackages.staging?
    xow fish emacs # Is the installation if these packages necessary?
  ];

  environment.variables = {
    GOROOT = [ "${pkgs.go.out}/share/go" ];
    # see: https://nixos.org/manual/nixos/unstable/index.html#sec-gpu-accel-vulkan
    environment.variables.VK_ICD_FILENAMES = "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json";
  };

  virtualisation = {
    virtualbox = {
      enable = true;
      host = {
        enable = true;
        enableExtensionPack = true; # Default
        enableHardening = true;
      };
      guest.enable = false; # Default
    };
    podman = {
      enable = true;
      dockerCompat = true;
      extraPackages = with pkgs; [
        podman-compose buildah kompose
      ];
    }
  };

  services = {
    dbus.enable = true;
    fstrim = {
      enable = true;
      interval = "weekly";
    };
    hardware = {
      xow.enable = true;
    };
    emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
    };
    openssh = {
      enable = true;
    };
    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint gutenprintBin
        # hplip hplipWithPlugin # HP
        brlaser brgenml1lpr brgenml1cupswrapper # Brother
      ];
    };
    xserver = {
      enable = true;
      videoDrivers = [ "amdgpu" ];
      layout = "de";
      xkbOptions = "caps:escape";
      synaptics.enable = false;
      displayManager.sddm = {
        enable = true;
        autoNumlock = true;
      };
      desktopManager.plasma5.enable = true;
    };
  };

  users.extraUsers = {
    bastian = {
      isNormalUser = true;
      description = "Bastian Hussi";
      group = "users";
      extraGroups = ["wheel" "networkmanager" "vboxusers" "adbusers"];
      shell = pkgs.fish;
      initialHashedPassword = "";
    };
  };

  nix.autoOptimiseStore = true;

  system = {
    autoUpgrade = {
      enable = true;
      channel = "https://nixos.org/channels/unstable"
    };
    system.stateVersion = "20.09";
  };
}

