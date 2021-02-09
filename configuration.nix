{ config, pkgs, ... }:

# https://codeberg.org/davidak/nixos-config
# https://gitlab.com/felschr/nixos-config

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

  sound.enable = true;

  hardware = {
    enableAllFirmware = true;
    cpu.amd.updateMicrocode = true;
    pulseaudio = {
      enable = true;
      package = pkgs.puluseaudioFull;
      support32Bit = true;
    };

    bluetooth = {
      enable = true;
    };

    video = {
      hidpi.enable = true;
    };

    opengl = {
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        amdvlk
        rocm-opencl-icd
        rocm-opencl-runtime
        mesa mesa_drivers
      ];
      # NOTE: requires unstable (or 21.03)
      extraPackages32 = with pkgs.driversi686Linux; [
        amdvlk mesa
      ];
    };
  };

  i18n = {
    defaultLocale = "de_DE.UTF-8";
    console = {
      useXkbConfig = true;
      font = "Lat2-Terminus32"
    };
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
    subpixel = {
      lcdfilter = "default";
      rgba = "rgb";
    };
    fonts = with pkgs; [
      corefonts
      roboto
      liberation_ttf
      dejavu_fonts
      noto-fonts
      noto-fonts-extra
      noto-fonts-cjk
      noto-fonts-emoji
      fira
      fira-code
      fira-code-symbols
      jetbrains-mono
      nerdfonts
    ];
    fontsconfig = {
      defaultFonts = {
        emoji = "Noto Color Emoji";
        monospace = ["Fira Code" "JetBrains Mono" "emoji"]
        sansSerif = ["Noto Sans" "DejaVu Sans" "Liberation Sans" "emoji"];
        serif = ["Noto Serif" "DejaVu Serif" "Liberation Serif" "emoji"];
      };
    };
  };

  programs = {
    fish = {
      enable = true;
    };
    steam.enable = true;
    adb.enable = true;
    gnome-terminal.enable = false;
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    tree wget git gnupg curl firefox unzip
    go openjdk nodejs watchman rustup
    rust-analyzer gopls go-tools
    exercism
    alacritty
    android-studio jetbrains.idea-community jetbrains.pycharm-community
    vscodium
    vivaldi vivaldi-widevine vivaldi-ffmpeg-codecs
    tor-browser-bundle-bin
    teams
    gnomeExtensions.appindicator
    gnome3.gnome-shell-extensions
    gnome3.dconf-editor
    gnome3.gnome-tweaks
    krita drawio gimp inkscape
    texlive.combined.scheme-medium
    insomnia dbeaver
    kubectl minikube
    wine-staging # vs. winePackages.staging?
    steam xow fish emacs # Is the installation if these packages necessary?
  ];

  environment.shellAliases = [
    cp = "cp -i"
    mv = "mv -i"
    rm = "rm -I"
    du = "du -sh"
    free = "free -h"

    python = "python3"
    py = "python"
    venv = "python -m venv"
    pdb = "python -m pdb"
  ]

  environment.gnome3.excludePackages = with pkgs.gnome3; [
    gnome-weather
    gnome-software
    totem
    epiphany
    geary
    cheese
  ];

  environment.variables = {
    GOROOT = [ "${pkgs.go.out}/share/go" ];
    # see: https://nixos.org/manual/nixos/unstable/index.html#sec-gpu-accel-vulkan
    VK_ICD_FILENAMES = "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json";
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = 48;
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

  documentation = {
    nixos.enable = false;
    man.enable = true; # Default
  };

  services = {
    dbus = {
      enable = true;
      packages = with pkgs; [
        gnome3.dconf
      ];
    };
    fwupd.enable = true;
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
      dpi = 192;
      modules = [ pkgs.xf86_input_wacom ];
      layout = "de";
      xkbOptions = "caps:escape";
      synaptics.enable = false;
      wacom.enable = true;
      displayManager.gdm = {
        enable = true;
        wayland = true; # Default?
      };
      desktopManager = {
        gnome3 = {
          enable = true;
        };
        xterm.enable = false;
      };
    };
  };

  users.extraUsers = {
    bastian = {
      isNormalUser = true;
      description = "Bastian Hussi";
      group = "users";
      extraGroups = ["wheel" "networkmanager" "audio" "vboxusers" "adbusers"];
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

