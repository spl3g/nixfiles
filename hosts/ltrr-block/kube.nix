{
  inputs,
  self,
  ...
}: {
  flake.nixosModules.ltrr-block = {
    pkgs,
    config,
    ...
  }: {
    age.secrets.kube-sops-key = {
      rekeyFile = ./secrets/kube-sops.key.age;
    };

    age.secrets.kube-token = {
      rekeyFile = ./secrets/kube-token.key.age;
    };

    networking.firewall = {
      allowedTCPPorts = [
        6443
        10250
      ];
      allowedUDPPorts = [
        8472
      ];
    };

    services.k3s = {
      enable = true;
      role = "server";
      agentTokenFile = config.age.secrets.kube-token.path;
      clusterInit = true;
      nodeIP = "10.1.1.2";
      nodeLabel = ["ltrr-block"];
      extraFlags = [
        "--disable traefik"
        "--disable servicelb"
        "--flannel-iface wg0"
      ];
      manifests = {
        flux-namespace = {
          content = {
            apiVersion = "v1";
            kind = "Namespace";
            metadata = {
              name = "flux-system";
              labels = {
                "pod-security.kubernetes.io/enforce" = "privileged";
              };
            };
          };
        };

        flux-operator = {
          content = {
            apiVersion = "helm.cattle.io/v1";
            kind = "HelmChart";
            metadata = {
              name = "flux-operator";
              namespace = "kube-system";
            };
            spec = {
              targetNamespace = "flux-system";
              createNamespace = false;
              chart = "oci://ghcr.io/controlplaneio-fluxcd/charts/flux-operator";
            };
          };
        };

        flux-instance = {
          content = {
            apiVersion = "fluxcd.controlplane.io/v1";
            kind = "FluxInstance";
            metadata = {
              name = "flux";
              namespace = "flux-system";
              annotations = {
                "fluxcd.controlplane.io/reconcileEvery" = "1h";
                "fluxcd.controlplane.io/reconcileTimeout" = "10m";
              };
            };
            spec = {
              distribution = {
                version = "2.x";
                registry = "ghcr.io/fluxcd";
                artifact = "oci://ghcr.io/controlplaneio-fluxcd/flux-operator-manifests";
              };
              components = [
                "source-controller"
                "source-watcher"
                "kustomize-controller"
                "helm-controller"
                "notification-controller"
                "image-reflector-controller"
                "image-automation-controller"
              ];
              cluster = {
                type = "kubernetes";
                multitenant = false;
                networkPolicy = true;
                domain = "cluster.local";
              };
              sync = {
                kind = "GitRepository";
                url = "https://codeberg.org/spl3g/kubefiles";
                ref = "refs/heads/main";
                path = "clusters/ltrr";
                interval = "1m";
              };
            };
          };
        };
      };
    };

    systemd.services.flux-sops-secret = {
      description = "Create Flux SOPS age key secret";
      after = ["k3s.service" "agenix.service"];
      requires = ["k3s.service"];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
        KUBECTL="${pkgs.kubectl}/bin/kubectl"

        # Retry loop inside the script
        for i in $(seq 1 60); do
          if $KUBECTL get --raw /healthz >/dev/null 2>&1; then
            break
          fi
          echo "Waiting for k3s API (attempt $i)..."
          sleep 5
        done

        # Now apply with validation disabled to avoid OpenAPI race
        $KUBECTL create namespace flux-system --dry-run=client -o yaml | \
          $KUBECTL apply --validate=false -f -

        $KUBECTL create secret generic sops-age \
          --namespace=flux-system \
          --from-file=age.agekey=${config.age.secrets.kube-sops-key.path} \
          --dry-run=client -o yaml | \
          $KUBECTL apply --validate=false -f -
      '';
      wantedBy = ["multi-user.target"];
    };
  };
}
