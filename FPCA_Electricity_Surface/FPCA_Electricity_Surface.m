% cd('C:/Users/Franziska Schulz/Desktop/JASA Data and Command Files');
clc
clear
load('LoadTSO.mat')

figure1 = figure('PaperType', 'a4letter');
axes1 = axes('Parent', figure1,...
    'ZTickLabel', {'100', '150', '200', '250', '300', '350'},...
    'YTickLabel', {'', '2012', '2011', '2010'},...
    'YTick', [0 365 712 1095],...
    'XTickLabel', {'00:00','06:00','12:00','18:00','24:00'},...
    'XTick', [0 25 50 75 100],...
    'FontSize', 14);
view(axes1, [-37.5 30]);
grid(axes1, 'on');
hold(axes1, 'all');

surf(Loadmat, 'Parent', axes1, 'EdgeColor', 'none');
xlabel('Time of day', 'FontSize', 20);
ylabel('Days', 'FontSize', 20);
zlabel('Load in 100 MW', 'FontSize', 20);